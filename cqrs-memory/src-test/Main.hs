module Main (main) where

import Test.Hspec ( hspec )
import Data.CQRS.Memory ( newEventStore
                        , newEventStream
                        , newStorage
                        , newSnapshotStore
                        )
import Data.CQRS.Test.TestKit ( mkEventStoreSpec
                              , mkEventStreamSpec
                              , mkRepositorySpec
                              , mkSnapshotStoreSpec
                              , TestKitSettings(..)
                              )

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
     mkRepositorySpec $ testKitSettings {
                            tksMakeContext = \c -> do
                              es <- newEventStore c
                              ss <- newSnapshotStore
                              return (es, ss)
                        }
     mkSnapshotStoreSpec $ testKitSettings {
                               tksMakeContext = \_ -> newSnapshotStore
                           }
