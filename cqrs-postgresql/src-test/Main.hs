{-# LANGUAGE OverloadedStrings #-}
module Main ( main ) where

import           Control.Applicative ((<$>))
import           Control.Monad (forM_)
import qualified Database.PostgreSQL.LibPQ as P
import           Data.CQRS.PostgreSQL ( newEventStore
                                      , newEventStream
                                      , newSnapshotStore
                                      )
import           Data.CQRS.PostgreSQL.Migrations
import           Data.CQRS.PostgreSQL.Metadata
import           Data.CQRS.PostgreSQL.Internal.MigrationSpec ( mkApplyMigrationsSpec )
import           Data.CQRS.PostgreSQL.Internal.QuerySpec ( mkQuerySpec )
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
        connectionString <- H.toConnectionString <$> H.createTemporaryDatabase url
        createPool (P.connectdb connectionString) P.finish 1 1 5
  -- Make sure we test both in the default schema and with a named schema
  forM_ [DefaultSchema, NamedSchema "foobar"] $ \schema -> do
    -- Setup for TestKit
    let testKitSettings = TestKitSettings
         { tksMakeContext = \_ -> return ()
         , tksSetUp = do
             connectionPool <- mkConnectionPool
             withResource connectionPool (`migrate` schema)
             return connectionPool
         , tksTearDown = destroyAllResources
         }
    -- Run the tests
    hspec $ do
       mkQuerySpec mkConnectionPool
       mkApplyMigrationsSpec mkConnectionPool
       mkSnapshotStoreSpec $ testKitSettings {
                                 tksMakeContext = (`newSnapshotStore` schema)
                             }
       mkEventStoreSpec $ testKitSettings {
                              tksMakeContext = (`newEventStore` schema)
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
