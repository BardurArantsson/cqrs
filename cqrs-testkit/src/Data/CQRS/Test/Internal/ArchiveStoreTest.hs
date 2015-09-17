{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
module Data.CQRS.Test.Internal.ArchiveStoreTest
    ( mkArchiveStoreSpec
    ) where

import           Control.DeepSeq (NFData)
import           Control.Monad (forM, forM_, replicateM)
import           Control.Monad.Trans.Reader (ask)
import           Control.Monad.IO.Class (liftIO)
import           Data.ByteString (ByteString)
import           Data.CQRS.Query
import           Data.CQRS.Types.ArchiveRef (ArchiveRef(..))
import           Data.CQRS.Types.ArchiveStore (ArchiveStore)
import qualified Data.CQRS.Types.ArchiveStore as AS
import           Data.CQRS.Types.EventStore (EventStore, esStoreEvents)
import           Data.CQRS.Test.Internal.ArchiveStoreUtils (readAllEventsFromArchiveStore)
import           Data.CQRS.Test.Internal.Scope (ScopeM, verify, randomUUID, mkRunScope)
import           Data.CQRS.Test.Internal.TestKitSettings
import           Data.CQRS.Test.Internal.Utils (randomByteString, chunkRandomly)
import           Data.Function (on)
import           Data.List (groupBy, sortBy)
import           Data.UUID.Types (UUID)
import qualified System.IO.Streams.List as SL
import qualified System.Random as R
import qualified Test.Hspec as Hspec
import           Test.Hspec (Expectation, Spec, describe, shouldBe)
import           Test.HUnit (assertBool)

-- Ambient data for test scope for each spec.
data Scope e = Scope { scopeArchiveStore :: ArchiveStore e
                     , scopeEventStore :: EventStore e
                     }

-- | Assert that two event streams are observationally equivalent
-- assuming that events may appear in any order wrt. aggregate ID, as
-- long as they respect the sequence number ordering.
shouldHaveEventsEquivalentTo :: (Eq e, Show e) => [(UUID, PersistedEvent e)] -> [(UUID, PersistedEvent e)] -> Expectation
shouldHaveEventsEquivalentTo actualEvents expectedEvents =
    assertBool message (actualEvents' == expectedEvents')
  where
    message = "Event sequence " ++ show actualEvents ++ " is not (observationally) equivalent to " ++ show expectedEvents
    -- Stable sort of the actualEvents by the aggregate ID.
    actualEvents' = sortBy (compare `on` fst) actualEvents
    -- Stable sort of the given ordering on the expectedEvents.
    expectedEvents' = sortBy (compare `on` fst) expectedEvents

-- Generate a sequence of 'n' events. All the events are generated
-- for the same aggregateId (which is chosen randomly).
generateFixedNumberOfEvents :: Int -> IO [(UUID, PersistedEvent ByteString)]
generateFixedNumberOfEvents n = do
  aggregateId <- R.randomIO
  events <- replicateM n generateEvent
  uuids <- replicateM n R.randomIO
  let persistedEvents = map (\(e, s, u) -> PersistedEvent e s u) $ zip3 events [0..] uuids
  return $ zip (repeat aggregateId) persistedEvents
  where
    generateEvent :: IO ByteString
    generateEvent = randomByteString 8

-- Store given events in exactly the order given.
storeEvents :: UUID -> [PersistedEvent e] -> ScopeM (Scope e) ()
storeEvents aggregateId events = do
  eventStore <- fmap scopeEventStore ask
  liftIO $ (esStoreEvents eventStore) aggregateId events

verifyArchives :: (Show e, Eq e) => Int -> [(UUID, PersistedEvent e)] -> ScopeM (Scope e) ()
verifyArchives expectedArchiveCount expectedPersistedEvents = do
  (archiveCount, persistedEvents) <- readAllEventsFromArchiveStore scopeArchiveStore
  verify $ do
    archiveCount `shouldBe` expectedArchiveCount
    persistedEvents `shouldHaveEventsEquivalentTo` expectedPersistedEvents

readArchive :: ArchiveRef -> ScopeM (Scope e) [(UUID, PersistedEvent e)]
readArchive archiveRef = do
  archiveStore <- fmap scopeArchiveStore ask
  liftIO $ (AS.asReadArchive archiveStore) archiveRef SL.toList

archiveEvents :: Int -> ScopeM (Scope e) (Maybe UUID)
archiveEvents archiveSize = do
  archiveStore <- fmap scopeArchiveStore ask
  liftIO $ (AS.asArchiveEvents archiveStore) archiveSize

rotateArchives :: Int -> ScopeM (Scope e) ()
rotateArchives archiveSize = do
  archiveStore <- fmap scopeArchiveStore ask
  liftIO $ (AS.rotateArchives archiveStore) archiveSize

-- Write out events from a given list in a randomized fashion.
storeEventsRandomized :: [(UUID, PersistedEvent e)] -> ScopeM (Scope e) ()
storeEventsRandomized events = do
    -- Chunk into random number of randomly sized batches.
    batches <- liftIO $ do
                 nChunks <- R.getStdRandom $ R.randomR (0, length events - 1)
                 chunkRandomly nChunks events
    -- Write each batch
    forM_ batches $ writeBatch
  where
    writeBatch batch = do
      -- Group by aggregate ID so we don't just write a single event at a time.
      let eventsByAggregateId = groupBy (\x y -> fst x == fst y) batch
      -- Write each group in a single call.
      forM_ eventsByAggregateId $ \eventGroup -> do
        let aggregateId = fst $ head eventGroup
        storeEvents aggregateId $ map snd eventGroup

-- Given test kit settings, create the full spec for testing the
-- archive store implementation against those settings.
mkArchiveStoreSpec :: TestKitSettings a (ArchiveStore ByteString, EventStore ByteString) -> Spec
mkArchiveStoreSpec testKitSettings = do

  describe "ArchiveStore.enumerateAllEvents " $ do

    it "basic enumeration should work" $ do
      -- Setup
      aggregateId <- randomUUID
      eventId0 <- randomUUID
      eventId1 <- randomUUID
      eventId2 <- randomUUID
      let expectedEvents = [ (aggregateId, PersistedEvent "3" 0 eventId0)
                           , (aggregateId, PersistedEvent "6" 1 eventId1)
                           , (aggregateId, PersistedEvent "9" 2 eventId2)
                           ]
      let expectedEvents' = map snd expectedEvents
      storeEvents aggregateId expectedEvents'
      -- Exercise
      (_, es) <- readAllEventsFromArchiveStore scopeArchiveStore
      -- Verify
      verify $ es `shouldHaveEventsEquivalentTo` expectedEvents

    it "enumeration for multiple aggregates should work" $ do
      -- Setup
      aggregateId0 <- randomUUID
      aggregateId1 <- randomUUID
      aggregateId2 <- randomUUID
      gpes0_0 <- genEvents aggregateId0 4 0
      gpes1_0 <- genEvents aggregateId1 5 0
      gpes0_1 <- genEvents aggregateId0 3 (0 + length gpes0_0)
      gpes2_0 <- genEvents aggregateId2 90 0
      gpes1_1 <- genEvents aggregateId1 5 (0 + length gpes1_0)
      -- Exercise
      (_, es) <- readAllEventsFromArchiveStore scopeArchiveStore
      -- Verify: All events are there and are in the correct order
      -- wrt. each individual aggregate.
      let gpes = concat [ gpes0_0, gpes0_1, gpes1_0, gpes1_1, gpes2_0 ]
      verify $ es `shouldHaveEventsEquivalentTo` gpes

  describe "ArciveStore module" $ do

    it "'archiveEvents' should move at most <archiveSize> events to the archive" $ do
      -- Setup
      persistedEvents <- liftIO $ generateFixedNumberOfEvents 6
      storeEventsRandomized persistedEvents
      -- Exercise
      (Just archiveId) <- archiveEvents 5
      -- Verify: Should have one event "left over" in current archive.
      currentArchiveEvents <- readArchive CurrentArchive
      verify $ length currentArchiveEvents `shouldBe` 1
      verify $ currentArchiveEvents `shouldHaveEventsEquivalentTo` (drop 5 $ persistedEvents)
      -- Verify archives (general)
      verifyArchives 1 persistedEvents
      -- Verify created archive
      archivedEvents <- readArchive (NamedArchive archiveId)
      verify $ archivedEvents `shouldHaveEventsEquivalentTo` (take 5 $ persistedEvents)

    it "'archiveEvents' should include all events if the number of events falls on the <archiveSize> boundary" $ do
      -- Setup
      persistedEvents <- liftIO $ generateFixedNumberOfEvents 5
      storeEventsRandomized persistedEvents
      -- Exercise
      (Just archiveId) <- archiveEvents 5
      -- Verify: Should not have any events in the <current> archive.
      currentArchiveEvents <- readArchive CurrentArchive
      verify $ length currentArchiveEvents `shouldBe` 0
      -- Verify archives
      verifyArchives 1 persistedEvents
      -- Verify contents of created archive
      archivedEvents <- readArchive (NamedArchive archiveId)
      verify $ archivedEvents `shouldHaveEventsEquivalentTo` persistedEvents

    it "'archiveEvents' should perform archival even if #unarchived events < archiveSize" $ do
      -- Setup
      persistedEvents <- liftIO $ generateFixedNumberOfEvents 7
      storeEventsRandomized persistedEvents
      -- Exercise
      (Just archiveId) <- archiveEvents 10
      -- Verify: Should not have any events in the <current> archive.
      currentArchiveEvents <- readArchive CurrentArchive
      verify $ length currentArchiveEvents `shouldBe` 0
      -- Verify archives
      verifyArchives 1 persistedEvents
      -- Verify contents of created archive
      archivedEvents <- readArchive (NamedArchive archiveId)
      verify $ archivedEvents `shouldHaveEventsEquivalentTo` persistedEvents

    it "'rotateArchives' should create as many archives as possible in a single invocation" $ do
      -- Setup
      persistedEvents <- liftIO $ generateFixedNumberOfEvents 17
      storeEventsRandomized persistedEvents
      -- Exercise
      rotateArchives 5
      -- Verify: Should not have any events in the <current> archive.
      currentArchiveEvents <- readArchive CurrentArchive
      verify $ length currentArchiveEvents `shouldBe` 2
      -- Verify archives
      verifyArchives 3 persistedEvents

  where
    -- Boilerplate avoidance
    it msg scope = Hspec.it msg $ runScope scope
    runScope = mkRunScope testKitSettings $ \a -> do
      -- Repository setup
      (archiveStore, eventStore) <- (tksMakeContext testKitSettings) a
      -- Build the ambient state.
      return $ Scope archiveStore eventStore

-- Publish a sequence of events.
publishEvents :: (NFData e, Show e) => UUID -> [PersistedEvent e] -> ScopeM (Scope e) ()
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
genEvents :: UUID -> Int -> Int -> ScopeM (Scope ByteString) [(UUID, PersistedEvent ByteString)]
genEvents aggregateId n i0 = do
  pes <- liftIO $ genEvents'
  publishEvents aggregateId pes
  return $ map (\pe -> (aggregateId, pe)) pes
  where
    genEvents' :: IO [PersistedEvent ByteString]
    genEvents' = do
      es <- replicateM n $ randomByteString 8
      pes <- forM (zip [0..] es) $ \(i, e) -> do
               eventId <- R.randomIO
               return $ PersistedEvent e (i0 + i) eventId
      return pes
