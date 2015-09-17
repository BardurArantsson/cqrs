{-# LANGUAGE OverloadedStrings #-}
module Data.CQRS.PostgreSQL.Migrations
    ( migrate
    ) where

import           Database.PostgreSQL.LibPQ (Connection)
import           Data.CQRS.PostgreSQL.Internal.Migration (applyMigrations, uuid)

-- | Apply all the necessary migrations to use any subset of the CQRS
-- functionality.
migrate :: Connection -> IO ()
migrate connection =
  applyMigrations connection
    [ (uuid "d0bf19c5-6bca-4bb0-8dd8-8e1340ec7503", sqlCreateEventTbl)
    , (uuid "1bbd4b4c-db0c-445c-90dd-19f7b725cac4", sqlCreateEventArchiveIdx)
    , (uuid "f1c4cec1-4b8c-4693-a48b-1a6f02e66741", sqlCreateArchiveTable)
    , (uuid "c7258b1c-bc3c-467a-bc1f-f63ad330651f", sqlCreateSnapshotTbl)
    ]
  where
    sqlCreateEventTbl =
        "CREATE TABLE event ( \
        \  aggregate_uuid UUID NOT NULL, \
        \  event_uuid UUID NOT NULL, \
        \  event_data BYTEA NOT NULL, \
        \  seq_no INTEGER NOT NULL, \
        \  \"timestamp\" BIGINT NOT NULL, \
        \  archive_uuid UUID, \
        \  PRIMARY KEY (aggregate_uuid, seq_no) \
        \)"
    sqlCreateEventArchiveIdx =
        "CREATE INDEX event_archive_idx ON event ( \
        \  archive_uuid \
        \)"
    sqlCreateArchiveTable =
        "CREATE TABLE archive ( \
        \  archive_uuid UUID NOT NULL, \
        \  prev_archive_uuid UUID DEFAULT NULL, \
        \  next_archive_uuid UUID DEFAULT NULL, \
        \  PRIMARY KEY (archive_uuid) \
        \)"
    sqlCreateSnapshotTbl =
        "CREATE TABLE snapshot ( \
        \  aggregate_uuid UUID PRIMARY KEY, \
        \  data BYTEA, version INTEGER \
        \)"
