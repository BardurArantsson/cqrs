{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.CQRS.Test.Internal.RepositoryTest
    ( mkRepositorySpec
    ) where

import           Control.DeepSeq (NFData)
import           Control.Monad (forM_, liftM)
import           Control.Monad.Trans.Reader (ask)
import           Control.Monad.IO.Class (liftIO)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString as B
import           Data.CQRS.Command (CommandT)
import qualified Data.CQRS.Command as C
import           Data.CQRS.Repository
import qualified Data.CQRS.Types.Chunk as Chunk
import           Data.CQRS.Types.Clock
import           Data.CQRS.Types.PersistedEvent
import           Data.CQRS.Types.EventStore (EventStore)
import           Data.CQRS.Types.SnapshotStore (nullSnapshotStore, SnapshotStore)
import           Data.CQRS.Test.Internal.AggregateAction (byteStringAggregateAction)
import           Data.CQRS.Test.Internal.Scope (ScopeM, verify, mkRunScope)
import           Data.CQRS.Test.Internal.TestKitSettings
import           Data.CQRS.Test.Internal.Utils (randomId)
import           Data.Int (Int32, Int64)
import           Data.IORef (IORef, newIORef, readIORef, atomicModifyIORef')
import qualified Data.List.NonEmpty as NEL
import           Data.Maybe (fromJust)
import qualified Test.Hspec as Hspec
import           Test.Hspec (Spec, shouldBe)
import           Test.HUnit (assertBool)

-- Ambient data for test scope for each spec.
data Scope i a e = Scope { scopeRepository :: Repository i a e
                         , scopePublishedEvents :: IORef [(i, e, Int32, Int64)]
                         }

-- Assert that the given list of events was published.
assertDidPublish :: (Show e, Eq e, Show i, Eq i) => [(i, e, Int32, Int64)] -> ScopeM (Scope i a e) ()
assertDidPublish expectedEvents = do
  publishedEventsRef <- fmap scopePublishedEvents $ ask
  verify $ do
    publishedEvents <- readIORef publishedEventsRef
    length publishedEvents `shouldBe` length expectedEvents
    forM_ (zip publishedEvents expectedEvents) $ uncurry shouldBe

-- Get the repository which is in scope.
getRepository :: ScopeM (Scope i a e) (Repository i a e)
getRepository = fmap scopeRepository ask

-- Run a command in scope.
runCommandT :: CommandT i a e IO r -> ScopeM (Scope i a e) r
runCommandT command = do
  repository <- getRepository
  liftIO $ C.runCommandT repository command

-- Create a new test scope runner from the test kit settings.
mkRunScope' :: Int -> TestKitSettings s (EventStore ByteString ByteString, SnapshotStore ByteString ByteString) -> (ScopeM (Scope ByteString ByteString ByteString) r -> IO r)
mkRunScope' snapshotFrequency testKitSettings = mkRunScope testKitSettings $ \a -> do
  -- We collect all events published by the repository for verification
  publishedEventsRef <- newIORef []
  let publish chunk =
        atomicModifyIORef' publishedEventsRef $ \events ->
            (events ++ NEL.toList (fmap (\(PersistedEvent e s ts) -> (i, e, s, ts)) events'), ())
          where (i, events') = Chunk.toList chunk
  -- Repository setup
  clock <- autoIncrementingClock 100 3
  (eventStore, snapshotStore) <- (tksMakeContext testKitSettings) a
  let settings =
        setSnapshotFrequency snapshotFrequency $
        setClock clock $
          defaultSettings
  let repository = newRepository settings byteStringAggregateAction eventStore snapshotStore publish
  -- Build the ambient state.
  return $ Scope repository publishedEventsRef

-- Given test kit settings, create the full spec for testing the
-- repository implementation against those settings.
mkRepositorySpec :: TestKitSettings a (EventStore ByteString ByteString, SnapshotStore ByteString ByteString) -> Spec
mkRepositorySpec testKitSettings = do
  -- We do each set of tests both *with* and *without* a snapshot
  -- store and with varying snapshot frequency. This should hopefully
  -- give us enough coverage against the handling of snapshots in the
  -- Repository portion of the code.
  forM_ [ 3, 5 ] $ \f -> do
    let fs = "frequency " ++ show f
    let s1 = "(snapshots; " ++ fs ++ ")"
    let s2 = "(null snapshots; " ++ fs ++ ")"
    mkSpec s1 (mkRunScope' f $ testKitSettings)
    mkSpec s2 (mkRunScope' f $ disableSnapshots testKitSettings)
  where
    disableSnapshots settings =
        settings { tksMakeContext = \a -> do
                        (eventStore, _) <- tksMakeContext settings a
                        return (eventStore, nullSnapshotStore)
                 }

mkSpec :: String -> (ScopeM (Scope ByteString ByteString ByteString) () -> IO ()) -> Spec
mkSpec suffix runScope = do

  describe "Repository" $ do

    it "should support creating an aggregate and returning its value" $ do
      -- Exercise
      (aggregateId, a) <- newAggregate ["3"]
      -- Should have updated aggregate value
      verify $ a `shouldBe` "3"
      -- Should have published appropriate event
      assertDidPublish [ (aggregateId, "3", 0, 100) ]

    it "should support creating an aggregate and loading it" $ do
      -- Exercise
      (aggregateId, _) <- newAggregate ["4"]
      -- Should have an updated aggregate value
      a <- loadAggregate aggregateId
      verify $ a `shouldBe` "4"
      -- Should have published appropriate event
      assertDidPublish [ (aggregateId, "4", 0, 100) ]

    it "should support publishing >1 events to an aggregate (1 txn)" $ do
      -- Exercise
      (aggregateId, _) <- newAggregate ["7", "1"]
      -- Should have an updated aggregate value
      a <- loadAggregate aggregateId
      verify $ a `shouldBe` "71"
      -- Should have published two events
      assertDidPublish [ (aggregateId, "7", 0, 100)
                       , (aggregateId, "1", 1, 103)
                       ]

    it "should support publishing >1 events to an aggregate (2 txns)" $ do
      -- Exercise: 1st transaction
      (aggregateId, _) <- newAggregate ["9"]
      -- Exercise: 2nd transaction
      _ <- runCommandT $ do
        C.updateAggregate aggregateId $ \_ -> do
          C.publishEvent $ "7"
      -- Should have an updated aggregate value.
      a <- loadAggregate aggregateId
      verify $ a `shouldBe` "97"
      -- Should have published two events.
      assertDidPublish [ (aggregateId, "9", 0, 100)
                       , (aggregateId, "7", 1, 103)
                       ]

    it "should support publishing a large number of events to an aggregate" $ do
      -- Setup
      let events = map B8.pack $ map show ([1.. 100] :: [Int])
      -- Exercise
      (aggregateId, _) <- newAggregate events
      -- Verify
      a <- loadAggregate aggregateId
      verify $ a `shouldBe` B.concat events

    it "should be possible to find an existing aggregate" $ do
      -- Setup
      (aggregateId, _) <- newAggregate ["xyzzy"]
      -- Exercise
      a <- findAggregate aggregateId
      -- Should have found it
      verify $ a `shouldBe` Just "xyzzy"

    it "should not be possible to find a non-existent aggregate" $ do
      -- Exercise
      aggregateId <- randomId
      a <- findAggregate aggregateId
      -- Should NOT have found anything
      verify $ a `shouldBe` Nothing

    it "should be possible to work with two different aggregates (serially) in a command" $ do
      -- Setup
      aggregateId0 <- randomId
      aggregateId1 <- randomId
      -- Exercise
      _ <- runCommandT $ do
        C.createAggregate aggregateId0 $ \_ -> do
          C.publishEvent "34"
        C.createAggregate aggregateId1 $ \_ -> do
          C.publishEvent "1"
        C.updateAggregate aggregateId0 $ \_ -> do
          C.publishEvent "5"
      -- Should have updated values for both aggregates
      a0 <- loadAggregate aggregateId0
      a1 <- loadAggregate aggregateId1
      verify $ a0 `shouldBe` "345"
      verify $ a1 `shouldBe` "1"
      -- Should have published events in order of publishing
      assertDidPublish [ (aggregateId0, "34", 0, 100)
                       , (aggregateId1,  "1", 0, 103)
                       , (aggregateId0,  "5", 1, 106)
                       ]

    it "'getter' function returns up-to-date values when updating an aggregate" $ do
      -- Setup
      (aggregateId, _) <- newAggregate ["x"]
      -- Exercise:
      Just (a, a') <- runCommandT $ do -- We'll assume pattern match will work, otherwise test fails
        C.updateAggregate aggregateId $ \get -> do
          a <- get
          C.publishEvent "y"
          a' <- get
          return (a, a')
      -- Should have received original value in a'
      verify $ a `shouldBe` "x"
      -- Should have received an updated value in a''
      verify $ a' `shouldBe` "xy"
      -- Should have published events
      assertDidPublish [ (aggregateId, "x", 0, 100)
                       , (aggregateId, "y", 1, 103)
                       ]

    it "'getter' function returns up-to-date values when creating an aggregate" $ do
      -- Setup
      aggregateId <- randomId
      -- Exercise
      (a, a') <- runCommandT $ do
        C.createAggregate aggregateId $ \get -> do
          a <- get  -- Should return Nothing
          C.publishEvent "x"
          a' <- get  -- Should reutrn (Just "x")
          return (a, a')
      -- Should have received Nothing in a' since aggregate didn't actually exist
      verify $ a `shouldBe` Nothing
      -- Should have received (Just "x") in a'' since we'd just published an "x" event
      verify $ a' `shouldBe` (Just "x")
      -- Should have published event
      assertDidPublish [ (aggregateId, "x", 0, 100) ]

  where
    -- Boilerplate avoidance
    it msg scope = Hspec.it msg $ runScope scope
    describe msg = Hspec.describe (msg ++ " " ++ suffix)

-- Create an aggregate with an initial series of events.
newAggregate :: (NFData a, NFData e) => [e] -> ScopeM (Scope ByteString a e) (ByteString, a)
newAggregate es = do
  -- Create new aggregate ID.
  aggregateId <- randomId
  -- Sanity check
  liftIO $ assertBool "List of initial events must be non-emtpy" (length es > 0)
  -- Create the aggregate with its initial list of events
  runCommandT $ do
    a <- C.createAggregate aggregateId $ \getAggregate -> do
      forM_ es $ C.publishEvent
      liftM fromJust $ getAggregate
    return (aggregateId, a)

-- Load an aggregate value.
loadAggregate :: i -> ScopeM (Scope i a e) a
loadAggregate aggregateId = findAggregate aggregateId >>= \case
  Nothing  -> error $ "loadAggregate: Missing expected aggregate"
  (Just a) -> return a

-- Get aggregate's value, if the aggregate exists
findAggregate :: i -> ScopeM (Scope i a e) (Maybe a)
findAggregate = runCommandT . C.readAggregate
