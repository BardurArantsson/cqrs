module Main (main) where

import           Data.CQRS.Memory
import           Data.CQRS.Test.TestKit ( mkEventStoreSpec
                                        , mkEventStreamSpec
                                        , mkKVStoreSpec
                                        , mkRepositorySpec
                                        , mkSnapshotStoreSpec
                                        , TestKitSettings(..)
                                        )
import           Test.Hspec ( hspec )

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
     mkEventStoreSpec $ testKitSettings {
                            tksMakeContext = newEventStore
                        }
     mkEventStreamSpec $ testKitSettings {
                             tksMakeContext = \c -> do
                               eventStream <- newEventStream c
                               eventStore <- newEventStore c
                               return (eventStream, eventStore)
                           }
     mkKVStoreSpec $ testKitSettings {
                         tksMakeContext = \_ -> do
                             kvs <- newKVStore
                             return kvs
                     }
     mkRepositorySpec $ testKitSettings {
                            tksMakeContext = \c -> do
                              es <- newEventStore c
                              ss <- newSnapshotStore
                              return (es, ss)
                        }
     mkSnapshotStoreSpec $ testKitSettings {
                               tksMakeContext = const newSnapshotStore
                           }
