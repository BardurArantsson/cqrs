{-# LANGUAGE OverloadedStrings #-}
module Main ( main ) where

import           Control.Applicative ((<$>))
import           Control.Monad (forM_)
import           Data.CQRS.PostgreSQL ( Schema(..)
                                      , newEventStore
                                      , newEventStream
                                      , newKVStore
                                      , newSnapshotStore
                                      , newStorageBackend
                                      )
import           Data.CQRS.PostgreSQL.Migrations
import           Data.CQRS.PostgreSQL.Internal.QuerySpec ( mkQuerySpec )
import           Data.CQRS.Test.TestKit ( mkEventStoreSpec
                                        , mkEventStreamSpec
                                        , mkKVStoreSpec
                                        , mkRepositorySpec
                                        , mkSnapshotStoreSpec
                                        , TestKitSettings(..)
                                        )
import           Data.Pool ( createPool
                           , withResource
                           , destroyAllResources
                           )
import           Database.PostgreSQL.Simple (connectPostgreSQL, close)
import qualified Database.PostgreSQL.Harness.Client as H
import           Test.Hspec

main :: IO ()
main = do
  -- HSpec has no easy way to get "other" command line parameters, so
  -- we'll just settle for a hardcoded value here.
  let url = "http://127.0.0.1:8900"
  -- Connection pool creation function. We use a fresh temporary
  -- database for every connection pool.
  let mkConnectionPool = do
        connectionString <- H.toConnectionString <$> H.createTemporaryDatabase url
        createPool (connectPostgreSQL connectionString) close 1 1 5
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
       mkKVStoreSpec $ testKitSettings {
                           tksMakeContext = \c -> do
                               withResource c (`migrateKVStore` schema)
                               kvs <- newKVStore c schema
                               return kvs
                       }
       mkRepositorySpec $ testKitSettings {
                              tksMakeContext = \c -> do
                                newStorageBackend c schema
                          }
