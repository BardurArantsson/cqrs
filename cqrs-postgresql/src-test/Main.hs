{-# LANGUAGE OverloadedStrings #-}
module Main ( main ) where

import           Control.Monad (forM_)
import qualified Database.PostgreSQL.LibPQ as P
import           Data.CQRS.PostgreSQL ( newEventStore
                                      , newEventStream
                                      , newSnapshotStore
                                      )
import           Data.CQRS.PostgreSQL.Migrations
import           Data.CQRS.PostgreSQL.Metadata
import           Data.CQRS.PostgreSQL.Internal.UtilsSpec ( mkUtilsSpec )
import           Data.CQRS.PostgreSQL.Internal.MigrationSpec ( mkApplyMigrationsSpec )
import           Data.CQRS.Test.TestKit ( mkEventStoreSpec
                                        , mkEventStreamSpec
                                        , mkRepositorySpec
                                        , mkSnapshotStoreSpec
                                        , TestKitSettings(..)
                                        )
import           Data.Pool ( createPool
                           , withResource
                           , destroyAllResources
                           )
import qualified Database.PostgreSQL.Harness.Client as H
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
  -- Make sure we test both in the default schema and with a named schema
  forM_ [DefaultSchema, NamedSchema "foobar"] $ \schema -> do
    -- Setup for TestKit
    let testKitSettings = TestKitSettings
         { tksMakeContext = \_ -> return ()
         , tksSetUp = do
             connectionPool <- mkConnectionPool
             withResource connectionPool (\c -> migrate c schema)
             return connectionPool
         , tksTearDown = destroyAllResources
         }
    -- Run the tests
    hspec $ do
       mkUtilsSpec mkConnectionPool
       mkApplyMigrationsSpec mkConnectionPool
       mkSnapshotStoreSpec $ testKitSettings {
                                 tksMakeContext = \c -> newSnapshotStore c schema
                             }
       mkEventStoreSpec $ testKitSettings {
                              tksMakeContext = \c -> newEventStore c schema
                          }
       mkEventStreamSpec $ testKitSettings {
                               tksMakeContext = \c -> do
                                 eventStream <- newEventStream c schema
                                 eventStore <- newEventStore c schema
                                 return (eventStream, eventStore)
                          }
       mkRepositorySpec $ testKitSettings {
                              tksMakeContext = \c -> do
                                es <- newEventStore c schema
                                ss <- newSnapshotStore c schema
                                return (es, ss)
                          }
