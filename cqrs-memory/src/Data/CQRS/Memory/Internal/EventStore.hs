module Data.CQRS.Memory.Internal.EventStore
    ( newEventStore
    ) where

import           Control.Concurrent.STM (STM, atomically)
import           Control.Concurrent.STM.TVar (TVar, readTVar, modifyTVar')
import           Control.Monad (when, forM_)
import           Control.Monad.STM (throwSTM)
import           Data.CQRS.Types.ArchiveRef
import           Data.CQRS.Types.EventStore (EventStore(..))
import           Data.CQRS.Types.PersistedEvent (PersistedEvent(..))
import           Data.CQRS.Types.StoreError (StoreError(..))
import           Data.CQRS.Memory.Internal.Storage
import qualified Data.Foldable as F
import           Data.List (nub, sortBy)
import           Data.Ord (comparing)
import           Data.Sequence (Seq, (><))
import qualified Data.Sequence as S
import           Data.UUID.Types (UUID)
import           System.IO.Streams (InputStream)
import qualified System.IO.Streams.List as SL
import qualified System.IO.Streams.Combinators as SC

storeEvents :: Storage e -> UUID -> [PersistedEvent e] -> IO ()
storeEvents (Storage store) aggregateId newEvents = atomically $ do
    -- Extract all existing events for this aggregate.
    events <- eventsByAggregateId store aggregateId
    let eventSequenceNumbers = F.toList $ fmap peSequenceNumber events
    -- Check for duplicates in the input list itself
    let newSequenceNumbers = F.toList $ fmap peSequenceNumber newEvents
    when ((nub newSequenceNumbers) /= newSequenceNumbers) $ throwSTM $ VersionConflict aggregateId
    -- Check for duplicate events
    forM_ newSequenceNumbers $ \newSequenceNumber -> do
      when (elem newSequenceNumber eventSequenceNumbers) $
           throwSTM $ VersionConflict aggregateId
    -- Check version numbers; this exists as a sanity check for tests
    let vE = lastStoredVersion $ F.toList events
    let v0 = if null newEvents then
                 vE + 1
               else
                 minimum $ map peSequenceNumber newEvents
    when (v0 /= vE + 1) $ error "Mismatched version numbers"
    -- Append to the list of all previous events
    modifyTVar' store addEvents
  where
    mkEvent persistedEvent = Event aggregateId persistedEvent CurrentArchive

    addEvents ms = ms { msEvents = (msEvents ms) >< (S.fromList $ map mkEvent newEvents) }

    lastStoredVersion [ ] = (-1)
    lastStoredVersion es  = maximum $ map peSequenceNumber es

retrieveEvents :: Storage e -> UUID -> Int -> (InputStream (PersistedEvent e) -> IO a) -> IO a
retrieveEvents (Storage store) aggregateId v0 f = do
  events <- fmap F.toList $ atomically $ eventsByAggregateId store aggregateId
  SL.fromList events >>= SC.filter (\e -> peSequenceNumber e > v0) >>= f

retrieveAllEvents :: Storage e -> (InputStream (UUID, PersistedEvent e) -> IO a) -> IO a
retrieveAllEvents (Storage store) f = do
  -- We won't bother with efficiency since this is only
  -- really used for debugging/tests.
  events <- fmap msEvents $ atomically $ readTVar store
  let eventList = F.toList events
  inputStream <- SL.fromList $ sortBy (comparing cf) eventList
  SC.map (\(Event aggregateId event _) -> (aggregateId, event)) inputStream >>= f
  where
    cf e = (eAggregateId e, peSequenceNumber $ ePersistedEvent e)


eventsByAggregateId :: TVar (Store e) -> UUID -> STM (Seq (PersistedEvent e))
eventsByAggregateId store aggregateId = do
  events <- readTVar store
  return $ fmap ePersistedEvent $ S.filter (\e -> aggregateId == eAggregateId e) $ msEvents events

-- | Create a memory-backend event store.
newEventStore :: Show e => Storage e -> IO (EventStore e)
newEventStore storage = do
  return $ EventStore
    { esStoreEvents = storeEvents storage
    , esRetrieveEvents = retrieveEvents storage
    , esRetrieveAllEvents = retrieveAllEvents storage
    }
