{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Data.CQRS.PostgreSQL.Internal.SnapshotStore
    ( newSnapshotStore
    ) where

import           Control.Monad.IO.Class (liftIO)
import           Data.ByteString (ByteString)
import           Data.CQRS.Types.Snapshot (Snapshot(..))
import           Data.CQRS.Types.SnapshotStore (SnapshotStore(..))
import           Data.CQRS.PostgreSQL.Internal.Utils
import           Data.CQRS.PostgreSQL.Internal.Tables
import           Data.CQRS.PostgreSQL.Metadata
import           Data.Int (Int32)
import           Data.Pool (Pool)
import           Database.PostgreSQL.LibPQ (Connection)
import           NeatInterpolation (text)
import qualified System.IO.Streams as Streams

writeSnapshot :: Pool Connection -> Tables -> ByteString -> Snapshot ByteString -> IO ()
writeSnapshot connectionPool tables aggregateId (Snapshot v d) =
  -- We ignore the possibility of data races with others trying to
  -- update the same snapshot since snapshots aren't important enough
  -- to merit the extra complexity.
  runTransactionP connectionPool $ do
    execSql upsertSnapshotSql
      [ SqlByteArray $ Just aggregateId
      , SqlByteArray $ Just d
      , SqlInt32 $ Just v
      ]
  where
    snapshotTable = tblSnapshot tables

    upsertSnapshotSql = [text|
      INSERT INTO $snapshotTable
                  ("aggregate_id", "data", "version")
           VALUES ($$1, $$2, $$3)
      ON CONFLICT ("aggregate_id")
        DO UPDATE
              SET "data" = $$2
                , "version" = $$3
    |]

readSnapshot :: Pool Connection -> Tables -> ByteString -> IO (Maybe (Snapshot ByteString))
readSnapshot connectionPool tables aggregateId = do
  runTransactionP connectionPool $ do
    -- Unpack columns from result.
    let unpackColumns :: [SqlValue] -> (ByteString, Int32)
        unpackColumns [ SqlByteArray (Just d)
                      , SqlInt32 (Just v) ] = (d, v)
        unpackColumns columns               = error $ badQueryResultMsg [show aggregateId] columns
    -- Run the query.
    r <- query selectSnapshotSql [SqlByteArray $ Just aggregateId] $ \inputStream -> do
           cs <- liftIO $ Streams.read inputStream
           return $ fmap unpackColumns cs
    case r of
      Just (d,v) -> return $ Just $ Snapshot v d
      Nothing    -> return Nothing

  where
    snapshotTable = tblSnapshot tables

    selectSnapshotSql = [text|
      SELECT "data", "version"
        FROM $snapshotTable
       WHERE "aggregate_id" = $$1
    |]

-- | Create an snapshot store backed by a PostgreSQL connection pool.
newSnapshotStore :: Pool Connection -> Schema -> IO (SnapshotStore ByteString ByteString)
newSnapshotStore connectionPool schema = do
  return $ SnapshotStore
    (writeSnapshot connectionPool tables)
    (readSnapshot connectionPool tables)
  where
    tables = mkTables schema

