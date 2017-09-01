{-# LANGUAGE OverloadedStrings #-}
module Data.CQRS.PostgreSQL.Internal.SnapshotStore
    ( newSnapshotStore
    ) where

import           Data.ByteString (ByteString)
import           Data.CQRS.Types.Snapshot (Snapshot(..))
import           Data.CQRS.Types.SnapshotStore (SnapshotStore(..))
import           Data.CQRS.PostgreSQL.Internal.Query
import           Data.CQRS.PostgreSQL.Internal.Transaction
import           Data.CQRS.PostgreSQL.Internal.Identifiers
import           Data.Pool (Pool)
import           Database.Peregrin.Metadata (Schema)
import           Database.PostgreSQL.Simple (Connection, Binary(..))

writeSnapshot :: Pool Connection -> Identifiers -> ByteString -> Snapshot ByteString -> IO ()
writeSnapshot connectionPool identifiers aggregateId (Snapshot v d) =
  -- We ignore the possibility of data races with others trying to
  -- update the same snapshot since snapshots aren't important enough
  -- to merit the extra complexity.
  runTransactionP connectionPool $
    execute upsertSnapshotSql ( snapshotTable
                              , Binary aggregateId
                              , Binary d, v
                              , Binary d, v
                              )
  where
    snapshotTable = tblSnapshot identifiers

    upsertSnapshotSql =
      "INSERT INTO ? \
      \            (\"aggregate_id\", \"data\", \"version\") \
      \     VALUES (?, ?, ?) \
      \ON CONFLICT (\"aggregate_id\") \
      \  DO UPDATE \
      \        SET \"data\" = ? \
      \          , \"version\" = ?"


readSnapshot :: Pool Connection -> Identifiers -> ByteString -> IO (Maybe (Snapshot ByteString))
readSnapshot connectionPool identifiers aggregateId =
  runTransactionP connectionPool $ do
    -- Run the query.
    r <- query1 selectSnapshotSql ( snapshotTable
                                  , Binary aggregateId
                                  )
    case r of
      Just (d,v) -> return $ Just $ Snapshot v d
      Nothing    -> return Nothing

  where
    snapshotTable = tblSnapshot identifiers

    selectSnapshotSql =
      "SELECT \"data\", \"version\" \
      \  FROM ? \
      \ WHERE \"aggregate_id\" = ?"

-- | Create an snapshot store backed by a PostgreSQL connection pool.
newSnapshotStore :: Pool Connection -> Schema -> IO (SnapshotStore ByteString ByteString)
newSnapshotStore connectionPool schema =
  return $ SnapshotStore
    (writeSnapshot connectionPool identifiers)
    (readSnapshot connectionPool identifiers)
  where
    identifiers = mkIdentifiers schema
