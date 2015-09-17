{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-| Module to import for the "Command" side of the application. -}
module Data.CQRS.Command
    ( -- Re-exports for user convenience:
      AggregateAction
    , Repository
      -- Exports:
    , CommandT
    , createAggregate
    , execCommandT
    , freshUUID
    , publishEvent
    , readAggregate
    , runCommandT
    , UnitOfWorkT
    , updateAggregate
    ) where

import           Control.DeepSeq (NFData)
import           Control.Monad (forM, join, liftM, void, when)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Trans.Class (MonadTrans(..), lift)
import           Control.Monad.Trans.State.Strict (StateT, runStateT, get, modify')
import           Control.Monad.Trans.Reader (ReaderT, runReaderT, ask)
import           Data.CQRS.Internal.Aggregate (Aggregate)
import qualified Data.CQRS.Internal.Aggregate as A
import           Data.CQRS.Internal.Repository
import           Data.CQRS.Types.AggregateAction (AggregateAction)
import           Data.CQRS.Types.EventStore (EventStore(..))
import           Data.CQRS.Types.PersistedEvent (PersistedEvent(..))
import           Data.CQRS.Types.Snapshot (Snapshot(..))
import           Data.CQRS.Types.SnapshotStore
import           Data.Foldable (forM_)
import           Data.Maybe (fromJust)
import           Data.UUID.Types (UUID)
import qualified System.IO.Streams.Combinators as SC

-- | Command monad transformer.
newtype CommandT a e m b = CommandT { unCommandT :: CommandM a e m b }
    deriving (Functor, Applicative, Monad)

instance MonadTrans (CommandT a e) where
  lift m = CommandT $ lift m

instance MonadIO m => MonadIO (CommandT a e m) where
    liftIO m = CommandT $ liftIO m

-- | Command monad. This is just a reader to provide ambient access to the Repository.
type CommandM a e = ReaderT (Command a e)

data Command a e =
    Command { commandRepository :: Repository a e
            }

-- | Unit of work monad transformer.
newtype UnitOfWorkT a e m b = UnitOfWorkT { unUnitOfWorkT :: UnitOfWorkM a e m b }
  deriving (Functor, Applicative, Monad)

instance MonadTrans (UnitOfWorkT a e) where
    lift m = UnitOfWorkT $ lift m

instance MonadIO m => MonadIO (UnitOfWorkT a e m) where
    liftIO m = UnitOfWorkT $ liftIO m

-- | Unit of work monad.
type UnitOfWorkM a e = StateT (UnitOfWork a e)

data UnitOfWork a e =
   UnitOfWork { uowAggregate :: Aggregate a e
              }

-- | Run a command against a repository.
runCommandT :: (MonadIO m) => Repository a e -> CommandT a e m b -> m b
runCommandT repository (CommandT command) = runReaderT command $ Command repository

-- | Run a command against a repository, ignoring the result.
execCommandT :: (MonadIO m) => Repository a e -> CommandT a e m b -> m ()
execCommandT repository = void . runCommandT repository

-- | Create a fresh UUID from the repository.
freshUUID :: (MonadIO m) => CommandT a e m UUID
freshUUID = CommandT $ do
  repository <- liftM commandRepository ask
  liftIO $ repositoryUUIDSupply repository

-- Write out all the changes for a given aggregate.
writeChanges :: (MonadIO m) => UUID -> Aggregate a e -> CommandT a e m ()
writeChanges aggregateId aggregate = CommandT $ do
  repository <- liftM commandRepository ask
  let snapshotStore = repositorySnapshotStore repository
  let eventStore = repositoryEventStore repository
  let publishEvents = repositoryPublishEvents repository
  -- Convert all the accumulated events to PersistedEvent
  versionedEvents <- forM (A.versionedEvents aggregate) $ \(v, e) -> do
    i <- liftIO $ repositoryUUIDSupply repository
    return $ PersistedEvent e v i
  -- We only care if new events were generated
  when (length versionedEvents > 0) $ do
    -- Commit events to event store.
    liftIO $ (esStoreEvents eventStore) aggregateId versionedEvents
    -- Publish events written so far.
    liftIO $ publishEvents (aggregateId, versionedEvents)
    -- Write out the snapshot (if applicable).
    forM_ (settingsSnapshotFrequency $ repositorySettings repository) $ \snapshotFrequency -> do
      forM_ (snapshotForAggregate snapshotFrequency) $ \snapshot -> do
        liftIO $ ssWriteSnapshot snapshotStore aggregateId snapshot
  where
    snapshotForAggregate maxDelta = join $ (flip fmap) (A.aggregateSnapshot aggregate) $ \(v, a) ->
      -- If we've advanced N events past the last snapshot, we create a
      -- new snapshot.
      let sv = A.aggregateSnapshotVersion aggregate in
        if (v - sv > maxDelta) then
          Just $ Snapshot v a
        else
          Nothing

-- Get the aggregateAction from the repository.
getAggregateAction :: (Monad m) => CommandT a e m (AggregateAction a e)
getAggregateAction = CommandT $ liftM (repositoryAggregateAction . commandRepository) ask

-- | Create a new aggregate using the supplied unit of work. Throws a
-- 'Data.CQRS.Types.VersionConflict' exception if there is already an
-- aggregate with the given aggregate ID. __NOTE__: The exception may
-- be thrown at the __END__ of the unit of work, and so any operations
-- in the unit of work that are lifted into the nested monad may be
-- performed regardless. (This is due to optimistic concurrency
-- control.)
createAggregate :: (MonadIO m, Monad m) => UUID -> (UnitOfWorkT a e (CommandT a e m) (Maybe a) -> UnitOfWorkT a e (CommandT a e m) b) -> CommandT a e m b
createAggregate aggregateId unitOfWork = do
  -- We use an "empty" aggregate state as the starting point
  -- here. We'll automatically conflict when trying to save if there
  -- is a conflict or if the aggregate already exists.  We cannot
  -- check this preemptively since aggregates may be created
  -- concurrently.
  (r, s) <- do
    aggregateAction <- getAggregateAction
    runStateT run $ UnitOfWork $ A.emptyAggregate aggregateAction
  -- Write out any changes.
  writeChanges aggregateId $ uowAggregate s
  -- Return the result of the computation
  CommandT $ return r
  where
    run = do
      let getter = liftM (A.aggregateValue . uowAggregate) get
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
updateAggregate :: (MonadIO m) => UUID -> (UnitOfWorkT a e (CommandT a e m) a -> UnitOfWorkT a e (CommandT a e m) b) -> CommandT a e m (Maybe b)
updateAggregate aggregateId unitOfWork = CommandT $ do
  aggregate <- getByIdFromEventStore aggregateId
  unCommandT $ case A.aggregateValue $ aggregate of
    Nothing -> do
      return Nothing
    Just _ -> do
      (r, s) <- runStateT run $ UnitOfWork aggregate
      writeChanges aggregateId $ uowAggregate s
      return $ Just r
  where
    run = do
      let getter = liftM (fromJust . A.aggregateValue . uowAggregate) get
      unUnitOfWorkT $ unitOfWork $ UnitOfWorkT getter

-- | Read value of an aggregate if it exists. If any update needs to
-- be performed on the aggregate, use of the 'getter' function (see
-- 'createAggregate' and 'updateAggregate') should be preferred.
readAggregate :: (MonadIO m) => UUID -> CommandT a e m (Maybe a)
readAggregate = (flip updateAggregate) id

-- Retrieve aggregate from event store.
getByIdFromEventStore :: (MonadIO m) => UUID -> CommandM a e m (Aggregate a e)
getByIdFromEventStore aggregateId = do
  r <- liftM commandRepository ask
  let es = repositoryEventStore r
  let ss = repositorySnapshotStore r
  let aa = repositoryAggregateAction r
  -- Start from a snapshot (if any).
  a' <- liftM (A.applySnapshot $ A.emptyAggregate aa) $ liftIO $ ssReadSnapshot ss $ aggregateId
  -- Apply any subsequent events.
  a'' <- liftIO $ esRetrieveEvents es aggregateId (A.aggregateVersion0 a') (SC.fold A.applyEvent a')
  -- Return the aggregate ref.
  return a''

-- | Publish event for the current aggregate.
publishEvent :: (MonadIO m, NFData a, NFData e) => e -> UnitOfWorkT a e (CommandT a e m) ()
publishEvent event = UnitOfWorkT $ do
  modify' (\s -> s { uowAggregate = A.publishEvent (uowAggregate s) event })
