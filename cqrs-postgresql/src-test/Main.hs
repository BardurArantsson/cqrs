module Main ( main ) where

import qualified Database.PostgreSQL.LibPQ as P
import           Data.CQRS.PostgreSQL (newEventStore, newSnapshotStore, newArchiveStore)
import           Data.CQRS.PostgreSQL.Migrations (migrate)
import           Data.CQRS.PostgreSQL.Internal.UtilsSpec (mkUtilsSpec)
import           Data.CQRS.PostgreSQL.Internal.MigrationSpec (mkApplyMigrationsSpec)
import           Data.CQRS.Test.TestKit (mkArchiveStoreSpec, mkEventStoreSpec, mkRepositorySpec, mkSnapshotStoreSpec, TestKitSettings(..))
import           Data.Pool (createPool, withResource, destroyAllResources)
import qualified Database.PostgreSQL.Harness.Client as H
import           System.Random (randomIO)
import           Test.Hspec

main :: IO ()
main = do
  -- HSpec has no easy way to get "other" command line parameters, so
  -- we'll just settle for a hardcoded value here.
  let url = "http://localhost:8900"
  -- Connection pool creation function. We use a fresh temporary
  -- database for every connection pool.
  let mkConnectionPool = do
        connectionString <- fmap H.toConnectionString $ H.createTemporaryDatabase url
        createPool (P.connectdb connectionString) P.finish 1 1 5
  -- Setup for TestKit
  let testKitSettings = TestKitSettings
       { tksMakeContext = \_ -> return ()
       , tksSetUp = do
           connectionPool <- mkConnectionPool
           withResource connectionPool migrate
           return connectionPool
       , tksTearDown = destroyAllResources
       }
  -- Run the tests
  hspec $ do
     mkUtilsSpec mkConnectionPool
     mkApplyMigrationsSpec mkConnectionPool
     mkArchiveStoreSpec $ testKitSettings {
                              tksMakeContext = \c -> do
                                as <- newArchiveStore randomIO c
                                es <- newEventStore c
                                return (as, es)
                            }
     mkSnapshotStoreSpec $ testKitSettings {
                               tksMakeContext = newSnapshotStore
                           }
     mkEventStoreSpec $ testKitSettings {
                            tksMakeContext = newEventStore
                        }
     mkRepositorySpec $ testKitSettings {
                            tksMakeContext = \c -> do
                              es <- newEventStore c
                              ss <- newSnapshotStore c
                              return (es, ss)
                        }
