{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.CQRS.Test.Internal.EventStreamTest
    ( mkEventStreamSpec
    ) where

import           Control.Monad ((>=>), forM_, replicateM)
import           Control.Monad.IO.Class (liftIO)
import           Data.ByteString (ByteString)
import           Data.CQRS.Internal.PersistedEvent
import           Data.CQRS.Test.Internal.Scope (ScopeM, verify, ask, mkRunScope)
import           Data.CQRS.Test.Internal.TestKitSettings
import           Data.CQRS.Test.Internal.Utils (randomId, randomByteString, chunkRandomly)
import qualified Data.CQRS.Types.Chunk as C
import           Data.CQRS.Types.Clock
import           Data.CQRS.Types.EventStore
import           Data.CQRS.Types.EventStream
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
                       , scopeClock :: Clock
                       }

-- | Assert that two event streams are observationally equivalent
-- assuming that events may appear in any order wrt. aggregate ID, as
-- long as they respect the sequence number ordering.
shouldHaveEventsEquivalentTo :: (Eq e, Show e, Show i, Ord i) => [(i, PersistedEvent e)] -> [(i, PersistedEvent e)] -> Expectation
shouldHaveEventsEquivalentTo actualEvents expectedEvents =
    assertBool message (actualEvents' == expectedEvents')
  where
    message = "Event sequence " ++ show actualEvents ++ " is not (observationally) equivalent to " ++ show expectedEvents
    -- Stable sort of the actualEvents by the aggregate ID.
    actualEvents' = sortBy (compare `on` fst) actualEvents
    -- Stable sort of the given ordering on the expectedEvents.
    expectedEvents' = sortBy (compare `on` fst) expectedEvents

-- Store given events in exactly the order given.
storeEvents :: i -> [PersistedEvent e] -> ScopeM (Scope i e) ()
storeEvents aggregateId events = do
  eventStore <- fmap scopeEventStore ask
  liftIO $ storeEvents' eventStore aggregateId events

storeEvents' :: EventStore i e -> i -> [PersistedEvent e] -> IO ()
storeEvents' eventStore aggregateId events =
  forM_ (C.fromList aggregateId events) $ esStoreEvents eventStore

readEventStream' :: StreamPosition -> (InputStream (StreamPosition, PersistedEvent' i e) -> IO a) -> ScopeM (Scope i e) a
readEventStream' startPosition f = do
  eventStream <- fmap scopeEventStream ask
  liftIO $ esReadEventStream eventStream startPosition f

readEventStream :: StreamPosition -> ScopeM (Scope i e) [(i, PersistedEvent e)]
readEventStream startPosition = do
  eventStream <- fmap scopeEventStream ask
  liftIO $ esReadEventStream eventStream startPosition (SC.map dropStreamPosition >=> SL.toList)
  where
    dropStreamPosition (_, e) = (pepAggregateId e, shrink e)

-- Given test kit settings, create the full spec for testing the event
-- stream implementation against those settings.
mkEventStreamSpec :: TestKitSettings a (EventStream ByteString ByteString, EventStore ByteString ByteString) -> Spec
mkEventStreamSpec testKitSettings =
  describe "EventStream implementation" $ do
    it "should support enumeration from the beginning" $ do
      -- Setup
      aggregateId <- randomId
      let expectedEvents = [ (aggregateId, PersistedEvent "3" 0 42)
                           , (aggregateId, PersistedEvent "6" 1 43)
                           , (aggregateId, PersistedEvent "9" 2 44)
                           ]
      let expectedEvents' = map snd expectedEvents
      storeEvents aggregateId expectedEvents'
      -- Exercise
      es <- readEventStream infimum
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
      es <- readEventStream infimum
      -- Verify: All events are there and are in the correct order
      -- wrt. each individual aggregate.
      let gpes = concat [ gpes0_0, gpes0_1, gpes1_0, gpes1_1, gpes2_0 ]
      verify $ es `shouldHaveEventsEquivalentTo` gpes

    it "should support resumption from a previously reached position" $ do
      -- Setup: Create a few events
      aggregateId <- randomId
      let expectedEvents = [ (aggregateId, PersistedEvent "3" 0 100)
                           , (aggregateId, PersistedEvent "6" 1 101)
                           , (aggregateId, PersistedEvent "9" 2 102)
                           , (aggregateId, PersistedEvent "12" 3 103)
                           , (aggregateId, PersistedEvent "15" 4 104)
                           ]
      storeEvents aggregateId $ map snd expectedEvents
      -- Setup: Read until we've seen two events
      p0 <- readEventStream' infimum $ \is -> do
                _           <- S.read is -- Read & ignore first event
                Just (p, _) <- S.read is -- Grab the position of the second event
                return p
      -- Exercise
      es <- readEventStream p0
      -- Verify: Should have everything except the first two events
      verify $ es `shouldHaveEventsEquivalentTo` drop 2 expectedEvents

  where
    -- Boilerplate avoidance
    it msg scope = Hspec.it msg $ runScope scope
    runScope = mkRunScope testKitSettings $ \a -> do
      -- Repository setup
      clock <- autoIncrementingClock 1000 1
      (archiveStore, eventStore) <- tksMakeContext testKitSettings a
      -- Build the ambient state.
      return $ Scope archiveStore eventStore clock


-- Publish a sequence of events.
publishEvents :: i -> [PersistedEvent e] -> ScopeM (Scope i e) ()
publishEvents aggregateId pes = do
  eventStore <- fmap scopeEventStore ask
  liftIO $ do
    chunks <- doChunk pes  -- Chunk list of events into randomly sized chunks.
    forM_ chunks $ \chunk -> do
      chunks' <- doChunk chunk    -- Re-chunk each chunk into (possibly) multiple command invokations.
      forM_ chunks' $ \chunk' ->
        storeEvents' eventStore aggregateId chunk'
  where
    doChunk xs = do
      n <- liftIO $ R.getStdRandom $ R.randomR (0, (length xs - 1) `div` 2)
      liftIO $ chunkRandomly n xs

-- Generate and publish a series of events to an aggregate.
genEvents :: forall i . i -> Int32 -> Int32 -> ScopeM (Scope i ByteString) [(i, PersistedEvent ByteString)]
genEvents aggregateId n i0 = do
  pes <- genEvents'
  publishEvents aggregateId pes
  return $ map (\pe -> (aggregateId, pe)) pes
  where
    genEvents' = do
      clock <- fmap scopeClock ask
      es <- liftIO $ replicateM (fromIntegral n) $ randomByteString 8
      tss <- liftIO $ replicateM (fromIntegral n) $ getMillis clock
      return $ map (\(i, e, ts) -> PersistedEvent e (i0 + i) ts) (zip3 [0..] es tss)
