module Main (main) where

import           Test.Hspec (hspec)
import           Data.CQRS.Memory (newEventStore, newArchiveStore, newStorage, newSnapshotStore)
import           Data.CQRS.Test.TestKit (mkArchiveStoreSpec, mkEventStoreSpec, mkRepositorySpec, mkSnapshotStoreSpec, TestKitSettings(..))
import           System.Random (randomIO)

-- Run all the test suites.
main :: IO ()
main = do
  -- Setup
  let testKitSettings = TestKitSettings
       { tksMakeContext = \_ -> return ()
       , tksSetUp = newStorage
       , tksTearDown = \_ -> return ()
       }
  -- Run the full test kit.
  hspec $ do
     mkArchiveStoreSpec $ testKitSettings {
                              tksMakeContext = \c -> do
                                as <- newArchiveStore randomIO c
                                es <- newEventStore c
                                return $ (as, es)
                            }
     mkEventStoreSpec $ testKitSettings {
                            tksMakeContext = newEventStore
                        }
     mkRepositorySpec $ testKitSettings {
                            tksMakeContext = \c -> do
                              es <- newEventStore c
                              ss <- newSnapshotStore
                              return (es, ss)
                        }
     mkSnapshotStoreSpec $ testKitSettings {
                               tksMakeContext = \_ -> newSnapshotStore
                           }
