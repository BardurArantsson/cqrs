module Data.CQRS.Memory.Internal.EventStore
    ( newEventStore
    ) where

import           Control.Monad (when, forM_)
import           Control.Monad.IO.Unlift (MonadUnliftIO, liftIO)
import           Data.CQRS.Internal.PersistedEvent
import           Data.CQRS.Types.Chunk (Chunk)
import qualified Data.CQRS.Types.Chunk as C
import           Data.CQRS.Types.EventStore (EventStore(..))
import           Data.CQRS.Types.StoreError (StoreError(..))
import           Data.CQRS.Memory.Internal.Storage
import qualified Data.Foldable as F
import           Data.Int (Int32)
import           Data.List (nub, sortBy)
import           Data.Ord (comparing)
import           Data.Sequence (Seq, (><))
import qualified Data.Sequence as S
import           Data.Typeable (Typeable)
import           UnliftIO.Exception (throwIO)
import           UnliftIO.IORef (atomicModifyIORef', readIORef, IORef)
import           UnliftIO.Streams (InputStream)
import qualified UnliftIO.Streams.List as SL
import qualified UnliftIO.Streams.Combinators as SC

storeEvents :: (Show i, Eq i, Typeable i, MonadUnliftIO m) => Storage i e -> Chunk i e -> m ()
storeEvents (Storage store) chunk = do
    -- There's a slight "inconsistency" here with runSanityCheck seeing a potentially
    -- different (earlier) set of events, but in the grand scheme of things it probably
    -- doesn't matter since they're only sanity checks.
    runSanityChecks
    -- Append to the list of all previous events
    atomicModifyIORef' store (\s -> (addEvents s, ()))
  where
    addEvents ms =
        -- Base timestamp
        let baseTimestamp = msCurrentTimestamp ms in
        -- Tag all the events with our wrapper
        let newTaggedEvents = map (\(e,ts) -> Event aggregateId e ts) (zip (F.toList newEvents) [baseTimestamp..]) in
        -- Update the storage
        ms { msEvents = msEvents ms >< S.fromList newTaggedEvents
           , msCurrentTimestamp = baseTimestamp + fromIntegral (length newTaggedEvents)
           }

    runSanityChecks = do
      -- Extract all existing events for this aggregate.
      events <- eventsByAggregateId store aggregateId
      -- Check for duplicates in the input list itself
      let newSequenceNumbers = F.toList $ fmap peSequenceNumber newEvents
      when (nub newSequenceNumbers /= newSequenceNumbers) $ liftIO $ throwIO $ VersionConflict aggregateId
      -- Check for duplicate events
      let eventSequenceNumbers = F.toList $ fmap peSequenceNumber events
      forM_ newSequenceNumbers $ \newSequenceNumber ->
        when (newSequenceNumber `elem` eventSequenceNumbers) $
             liftIO $ throwIO $ VersionConflict aggregateId
      -- Check version numbers; this exists as a sanity check for tests
      let vE = lastStoredVersion $ F.toList events
      let v0 = if null newEvents then
                   vE + 1
                 else
                   minimum $ fmap peSequenceNumber newEvents
      when (v0 /= vE + 1) $ error "Mismatched version numbers"

    lastStoredVersion [ ] = -1
    lastStoredVersion es  = maximum $ map peSequenceNumber es

    (aggregateId, newEvents) = C.toList chunk

retrieveEvents :: (Eq i, MonadUnliftIO m) => Storage i e -> i -> Int32 -> (InputStream (PersistedEvent e) -> m a) -> m a
retrieveEvents (Storage store) aggregateId v0 f = do
  events <- F.toList <$> eventsByAggregateId store aggregateId
  SL.fromList events >>= SC.filter (\e -> peSequenceNumber e > v0) >>= f

retrieveAllEvents :: (Ord i, MonadUnliftIO m) => Storage i e -> (InputStream (PersistedEvent' i e) -> m a) -> m a
retrieveAllEvents (Storage store) f = do
  -- We won't bother with efficiency since this is only
  -- really used for debugging/tests.
  events <- msEvents <$> readIORef store
  let eventList = F.toList events
  inputStream <- liftIO $ SL.fromList $ sortBy (comparing cf) eventList
  SC.map (\(Event i event _) -> grow i event) inputStream >>= f
  where
    cf e = (eAggregateId e, peSequenceNumber $ ePersistedEvent e)

eventsByAggregateId :: (Eq i, MonadUnliftIO m) => IORef (Store i e) -> i -> m (Seq (PersistedEvent e))
eventsByAggregateId store aggregateId = do
  events <- readIORef store
  return $ fmap ePersistedEvent $ S.filter (\e -> aggregateId == eAggregateId e) $ msEvents events

-- | Create a memory-backend event store.
newEventStore :: (Show i, Ord i, Typeable i) => Storage i e -> IO (EventStore i e)
newEventStore storage =
  return EventStore
    { esStoreEvents = storeEvents storage
    , esRetrieveEvents = retrieveEvents storage
    , esRetrieveAllEvents = retrieveAllEvents storage
    }
