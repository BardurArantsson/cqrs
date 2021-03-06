{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-| Module to import for the "Command" side of the application. -}
module Data.CQRS.Command
    ( -- Re-exports for user convenience:
      AggregateAction
    , Repository
      -- Exports:
    , CommandT
    , createAggregate
    , execCommandT
    , publishEvent
    , readAggregate
    , runCommandT
    , UnitOfWorkT
    , updateAggregate
    ) where

import           Control.DeepSeq (NFData)
import           Control.Monad (join, void)
import           Control.Monad.IO.Class (MonadIO(..))
import           Control.Monad.IO.Unlift (MonadUnliftIO(..), liftIO, wrappedWithRunInIO)
import           Control.Monad.Trans.Class (MonadTrans(..), lift)
import           Control.Monad.Trans.Reader (ReaderT(..), runReaderT, ask)
import           Data.CQRS.Internal.Aggregate (Aggregate)
import qualified Data.CQRS.Internal.Aggregate as A
import           Data.CQRS.Internal.Repository
import           Data.CQRS.Internal.UState
import           Data.CQRS.Types.AggregateAction (AggregateAction)
import qualified Data.CQRS.Types.Chunk as C
import           Data.CQRS.Types.Clock (Clock, getMillis)
import           Data.CQRS.Types.EventStore (EventStore(..))
import           Data.CQRS.Types.PersistedEvent (PersistedEvent(..))
import           Data.CQRS.Types.Snapshot (Snapshot(..))
import           Data.CQRS.Types.SnapshotStore
import           Data.CQRS.Types.StorageBackend (StorageBackend(..))
import           Data.Foldable (forM_)
import           Data.Maybe (fromJust)
import qualified UnliftIO.Streams.Combinators as SC

-- | Command monad transformer.
newtype CommandT i a e m b = CommandT { unCommandT :: ReaderT (CommandE i a e) m b }
    deriving (Functor, Applicative, Monad)

instance MonadTrans (CommandT i a e) where
  lift m = CommandT $ lift m

instance MonadIO m => MonadIO (CommandT i a e m) where
    liftIO m = CommandT $ liftIO m

instance MonadUnliftIO m => MonadUnliftIO (CommandT i a e m) where
  withRunInIO = wrappedWithRunInIO CommandT unCommandT

-- | Environment for CommandT.
data CommandE i a e = CommandE
  { commandRepository :: Repository i a e
  , commandEventStore :: EventStore i e
  , commandSnapshotStore :: SnapshotStore i a
  }

-- | Unit of work monad transformer.
newtype UnitOfWorkT a e m b = UnitOfWorkT { unUnitOfWorkT :: UStateT (UnitOfWorkE a e) m b }
  deriving (Functor, Applicative, Monad)

instance MonadTrans (UnitOfWorkT a e) where
    lift m = UnitOfWorkT $ lift m

instance MonadIO m => MonadIO (UnitOfWorkT a e m) where
    liftIO m = UnitOfWorkT $ liftIO m

instance MonadUnliftIO m => MonadUnliftIO (UnitOfWorkT a e m) where
  withRunInIO = wrappedWithRunInIO UnitOfWorkT unUnitOfWorkT

-- | Environment for UnitOfWorkT.
data UnitOfWorkE a e =
   UnitOfWorkE { uowAggregate :: Aggregate a e
               , uowClock :: Clock
               }

-- | Run a command against a repository.
runCommandT :: Repository i a e -> CommandT i a e m b -> m b
runCommandT repository (CommandT command) = runReaderT command env
  where
    storageBackend = repositoryStorageBackend repository
    eventStore = sbEventStore storageBackend
    snapshotStore = sbSnapshotStore storageBackend
    env = CommandE repository eventStore snapshotStore

-- | Run a command against a repository, ignoring the result.
execCommandT :: (MonadUnliftIO m) => Repository i a e -> CommandT i a e m b -> m ()
execCommandT repository = void . runCommandT repository

-- Write out all the changes for a given aggregate.
writeChanges :: (MonadUnliftIO m) => i -> Aggregate a e -> CommandT i a e m ()
writeChanges aggregateId aggregate = CommandT $ do
  repository <- fmap commandRepository ask
  snapshotStore <- fmap commandSnapshotStore ask
  eventStore <- fmap commandEventStore ask
  let publishEvents = repositoryPublishEvents repository
  -- Convert all the accumulated events to a chunk
  let maybeChunk = C.fromList aggregateId $ map (\(v, (e, ts)) -> PersistedEvent e v ts) (A.versionedEvents aggregate)
  -- We only care if new events were generated
  forM_ maybeChunk $ \chunk -> do
    -- Commit events to event store.
    esStoreEvents eventStore chunk
    -- Publish events written so far.
    publishEvents chunk
    -- Write out the snapshot (if applicable).
    forM_ (settingsSnapshotFrequency $ repositorySettings repository) $ \snapshotFrequency ->
      forM_ (snapshotForAggregate $ fromIntegral snapshotFrequency) $ \snapshot ->
        ssWriteSnapshot snapshotStore aggregateId snapshot
  where
    snapshotForAggregate maxDelta = join $ flip fmap (A.aggregateSnapshot aggregate) $ \(v, a) ->
      -- If we've advanced N events past the last snapshot, we create a
      -- new snapshot.
      let sv = A.aggregateSnapshotVersion aggregate in
        if v - sv > maxDelta then
          Just $ Snapshot v a
        else
          Nothing

-- Get the aggregateAction from the repository.
getAggregateAction :: (Monad m) => CommandT i a e m (AggregateAction a e)
getAggregateAction = CommandT $ fmap (repositoryAggregateAction . commandRepository) ask

-- Get the system clock from the repository.
getClock :: (Monad m) => CommandT i a e m Clock
getClock = CommandT $ fmap (settingsClock . repositorySettings .commandRepository) ask

-- | Create a new aggregate using the supplied unit of work. Throws a
-- 'Data.CQRS.Types.VersionConflict' exception if there is already an
-- aggregate with the given aggregate ID. __NOTE__: The exception may
-- be thrown at the __END__ of the unit of work, and so any operations
-- in the unit of work that are lifted into the nested monad may be
-- performed regardless. (This is due to optimistic concurrency
-- control.)
createAggregate :: forall a e i m b . (MonadUnliftIO m) => i -> (UnitOfWorkT a e (CommandT i a e m) (Maybe a) -> UnitOfWorkT a e (CommandT i a e m) b) -> CommandT i a e m b
createAggregate aggregateId unitOfWork = do
  -- We use an "empty" aggregate state as the starting point
  -- here. We'll automatically conflict when trying to save if there
  -- is a conflict or if the aggregate already exists.  We cannot
  -- check this preemptively since aggregates may be created
  -- concurrently.
  (r, s) <- do
    aggregateAction <- getAggregateAction
    clock <- getClock
    runUStateT run $ UnitOfWorkE (A.emptyAggregate aggregateAction) clock
  -- Write out any changes.
  writeChanges aggregateId $ uowAggregate s
  -- Return the result of the computation
  CommandT $ return r
  where
    run = do
      let getter = fmap (A.aggregateValue . uowAggregate) getU
      unUnitOfWorkT $ unitOfWork $ UnitOfWorkT getter

-- | Update aggregate with the supplied unit of work. The unit of work
-- is given a function to get the current value of the aggregate.
-- Returns the value returned by the unit of work, or 'Nothing' if
-- there was no aggregate with the given ID. Throws a
-- 'Data.CQRS.Types.VersionConflict' exception if a version conflict
-- occurs during commit. __NOTE__: The exception may be thrown at the
-- END of the unit of work, and so any operations in the unit of work
-- that are lifted into the nested monad may be performed
-- regardless. (This is due to optimistic concurrency control.)
updateAggregate :: (MonadUnliftIO m) => i -> (UnitOfWorkT a e (CommandT i a e m) a -> UnitOfWorkT a e (CommandT i a e m) b) -> CommandT i a e m (Maybe b)
updateAggregate aggregateId unitOfWork = do
  clock <- getClock
  CommandT $ do
    aggregate <- getByIdFromEventStore aggregateId
    unCommandT $ case A.aggregateValue aggregate of
      Nothing ->
        return Nothing
      Just _ -> do
        (r, s) <- runUStateT run $ UnitOfWorkE aggregate clock
        writeChanges aggregateId $ uowAggregate s
        return $ Just r
  where
    run = do
      let getter = fmap (fromJust . A.aggregateValue . uowAggregate) getU
      unUnitOfWorkT $ unitOfWork $ UnitOfWorkT getter

-- | Read value of an aggregate if it exists. If any update needs to
-- be performed on the aggregate, 'createAggregate' and 'updateAggregate'
-- should be used.
readAggregate :: (MonadUnliftIO m) => i -> CommandT i a e m (Maybe a)
readAggregate = CommandT . fmap A.aggregateValue . getByIdFromEventStore

-- Retrieve aggregate from event store.
getByIdFromEventStore :: (MonadUnliftIO m) => i -> ReaderT (CommandE i a e) m (Aggregate a e)
getByIdFromEventStore aggregateId = do
  r <- fmap commandRepository ask
  es <- fmap commandEventStore ask
  ss <- fmap commandSnapshotStore ask
  let aa = repositoryAggregateAction r
  a' <- (A.applySnapshot $ A.emptyAggregate aa) <$> ssReadSnapshot ss aggregateId
  lift $ esRetrieveEvents es aggregateId (A.aggregateVersion0 a') (SC.fold A.applyEvent a')

-- | Publish event for the current aggregate.
publishEvent :: (MonadUnliftIO m, NFData a, NFData e) => e -> UnitOfWorkT a e (CommandT i a e m) ()
publishEvent event = UnitOfWorkT $ do
  env <- getU
  -- Apply updates
  a <- fmap uowAggregate getU
  a' <- fmap (A.publishEvent a event) $ getMillis $ uowClock env
  -- Update the aggregate
  modifyU $ \s -> s { uowAggregate = a' }
