{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.CQRS.Test.Internal.EventStreamTest
    ( mkEventStreamSpec
    ) where

import           Control.Monad (forM_, replicateM)
import           Control.Monad.Trans.Reader (ask)
import           Control.Monad.IO.Class (liftIO)
import           Data.ByteString (ByteString)
import           Data.CQRS.Test.Internal.Scope (ScopeM, verify, mkRunScope)
import           Data.CQRS.Test.Internal.TestKitSettings
import           Data.CQRS.Test.Internal.Utils (randomId, randomByteString, chunkRandomly)
import           Data.CQRS.Types.EventStore
import           Data.CQRS.Types.EventStream
import           Data.CQRS.Types.PersistedEvent
import           Data.CQRS.Types.StreamPosition
import           Data.Function (on)
import           Data.Int (Int32)
import           Data.List (sortBy)
import           System.IO.Streams (InputStream)
import qualified System.IO.Streams as S
import qualified System.IO.Streams.List as SL
import qualified System.IO.Streams.Combinators as SC
import qualified System.Random as R
import qualified Test.Hspec as Hspec
import           Test.Hspec (Expectation, Spec, describe)
import           Test.HUnit (assertBool)

-- Ambient data for test scope for each spec.
data Scope i e = Scope { scopeEventStream :: EventStream i e
                       , scopeEventStore :: EventStore i e
                       }

-- | Assert that two event streams are observationally equivalent
-- assuming that events may appear in any order wrt. aggregate ID, as
-- long as they respect the sequence number ordering.
shouldHaveEventsEquivalentTo :: (Eq e, Show e, Show i, Ord i) => [(i, PersistedEvent i e)] -> [(i, PersistedEvent i e)] -> Expectation
shouldHaveEventsEquivalentTo actualEvents expectedEvents =
    assertBool message (actualEvents' == expectedEvents')
  where
    message = "Event sequence " ++ show actualEvents ++ " is not (observationally) equivalent to " ++ show expectedEvents
    -- Stable sort of the actualEvents by the aggregate ID.
    actualEvents' = sortBy (compare `on` fst) actualEvents
    -- Stable sort of the given ordering on the expectedEvents.
    expectedEvents' = sortBy (compare `on` fst) expectedEvents

-- Store given events in exactly the order given.
storeEvents :: i -> [PersistedEvent i e] -> ScopeM (Scope i e) ()
storeEvents aggregateId events = do
  eventStore <- fmap scopeEventStore ask
  liftIO $ (esStoreEvents eventStore) aggregateId events

readEventStream' :: Maybe StreamPosition -> (InputStream (StreamPosition, PersistedEvent i e) -> IO a) -> ScopeM (Scope i e) a
readEventStream' maybeStartPosition f = do
  eventStream <- fmap scopeEventStream ask
  liftIO $ (esReadEventStream eventStream) maybeStartPosition f

readEventStream :: Maybe StreamPosition -> ScopeM (Scope i e) [(i, PersistedEvent i e)]
readEventStream maybeStartPosition = do
  eventStream <- fmap scopeEventStream ask
  liftIO $ (esReadEventStream eventStream) maybeStartPosition (SC.map dropStreamPosition) >>= SL.toList
  where
    dropStreamPosition (_, e) = (peAggregateId e, e)

-- Given test kit settings, create the full spec for testing the event
-- stream implementation against those settings.
mkEventStreamSpec :: TestKitSettings a (EventStream ByteString ByteString, EventStore ByteString ByteString) -> Spec
mkEventStreamSpec testKitSettings = do

  describe "EventStream implementation" $ do

    it "should support enumeration from the beginning" $ do
      -- Setup
      aggregateId <- randomId
      let expectedEvents = [ (aggregateId, PersistedEvent "3" 0 aggregateId)
                           , (aggregateId, PersistedEvent "6" 1 aggregateId)
                           , (aggregateId, PersistedEvent "9" 2 aggregateId)
                           ]
      let expectedEvents' = map snd expectedEvents
      storeEvents aggregateId expectedEvents'
      -- Exercise
      es <- readEventStream Nothing
      -- Verify
      verify $ es `shouldHaveEventsEquivalentTo` expectedEvents

    it "should support enumeration from the beginning (multiple aggregates)" $ do
      -- Setup
      aggregateId0 <- randomId
      aggregateId1 <- randomId
      aggregateId2 <- randomId
      gpes0_0 <- genEvents aggregateId0 4 0
      gpes1_0 <- genEvents aggregateId1 5 0
      gpes0_1 <- genEvents aggregateId0 3 (0 + fromIntegral (length gpes0_0))
      gpes2_0 <- genEvents aggregateId2 90 0
      gpes1_1 <- genEvents aggregateId1 5 (0 + fromIntegral (length gpes1_0))
      -- Exercise
      es <- readEventStream Nothing
      -- Verify: All events are there and are in the correct order
      -- wrt. each individual aggregate.
      let gpes = concat [ gpes0_0, gpes0_1, gpes1_0, gpes1_1, gpes2_0 ]
      verify $ es `shouldHaveEventsEquivalentTo` gpes

    it "should support resumption from a previously reached position" $ do
      -- Setup: Create a few events
      aggregateId <- randomId
      let expectedEvents = [ (aggregateId, PersistedEvent "3" 0 aggregateId)
                           , (aggregateId, PersistedEvent "6" 1 aggregateId)
                           , (aggregateId, PersistedEvent "9" 2 aggregateId)
                           , (aggregateId, PersistedEvent "12" 3 aggregateId)
                           , (aggregateId, PersistedEvent "15" 4 aggregateId)
                           ]
      storeEvents aggregateId $ map snd expectedEvents
      -- Setup: Read until we've seen two events
      p0 <- readEventStream' Nothing $ \is -> do
                _           <- S.read is -- Read & ignore first event
                Just (p, _) <- S.read is -- Grab the position of the second event
                return p
      -- Exercise
      es <- readEventStream $ Just p0
      -- Verify: Should have everything except the first two events
      verify $ es `shouldHaveEventsEquivalentTo` (drop 2 expectedEvents)

  where
    -- Boilerplate avoidance
    it msg scope = Hspec.it msg $ runScope scope
    runScope = mkRunScope testKitSettings $ \a -> do
      -- Repository setup
      (archiveStore, eventStore) <- (tksMakeContext testKitSettings) a
      -- Build the ambient state.
      return $ Scope archiveStore eventStore

-- Publish a sequence of events.
publishEvents :: i -> [PersistedEvent i e] -> ScopeM (Scope i e) ()
publishEvents aggregateId pes = do
  eventStore <- fmap scopeEventStore ask
  liftIO $ do
    batches <- doChunk pes  -- Chunk list of events into randomly sized batches.
    forM_ batches $ \batch -> do
      chunks <- doChunk batch    -- Re-chunk each batch into (possibly) multiple command invokations.
      forM_ chunks $ \chunk -> do
        (esStoreEvents eventStore) aggregateId chunk
  where
    doChunk xs = do
      n <- liftIO $ R.getStdRandom $ R.randomR (0, (length xs - 1) `div` 2)
      liftIO $ chunkRandomly n xs

-- Generate and publish a series of events to an aggregate.
genEvents :: forall i . i -> Int32 -> Int32 -> ScopeM (Scope i ByteString) [(i, PersistedEvent i ByteString)]
genEvents aggregateId n i0 = do
  pes <- liftIO $ genEvents'
  publishEvents aggregateId pes
  return $ map (\pe -> (aggregateId, pe)) pes
  where
    genEvents' :: IO [PersistedEvent i ByteString]
    genEvents' = do
      es <- replicateM (fromIntegral n) $ randomByteString 8
      return $ map (\(i, e) -> PersistedEvent e (i0 + i) aggregateId) (zip [0..] es)
