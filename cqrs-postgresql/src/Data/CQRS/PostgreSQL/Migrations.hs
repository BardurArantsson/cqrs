{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Data.CQRS.PostgreSQL.Migrations
    ( migrate
    ) where

import           Database.PostgreSQL.LibPQ (Connection)
import           Data.CQRS.PostgreSQL.Internal.Migration
import           Data.CQRS.PostgreSQL.Internal.Tables
import           Data.CQRS.PostgreSQL.Metadata
import           NeatInterpolation (text)

-- | Apply all the necessary migrations to use any subset of the CQRS
-- functionality.
migrate :: Connection -> Schema -> IO ()
migrate connection schema =
  applyMigrations connection schema
    [ ("d0bf19c5-6bca-4bb0-8dd8-8e1340ec7503", sqlCreateEventTbl)
    , ("1bbd4b4c-db0c-445c-90dd-19f7b725cac4", sqlCreateLogicalTimestampIdx)
    , ("c7258b1c-bc3c-467a-bc1f-f63ad330651f", sqlCreateSnapshotTbl)
    ]
  where
    tbl = mkTables schema
    eventTable = tblEvent tbl
    snapshotTable = tblSnapshot tbl

    sqlCreateEventTbl = [text|
      CREATE TABLE $eventTable (
        "aggregate_id" BYTEA NOT NULL,
        "event_data" BYTEA NOT NULL,
        "seq_no" INTEGER NOT NULL,
        "timestamp" BIGINT NOT NULL,
        "l_timestamp" BIGSERIAL NOT NULL UNIQUE,
        PRIMARY KEY ("aggregate_id", "seq_no")
      )
    |]

    sqlCreateLogicalTimestampIdx = [text|
      CREATE INDEX "l_timestamp" ON $eventTable (
        "l_timestamp"
      )
    |]

    sqlCreateSnapshotTbl = [text|
      CREATE TABLE $snapshotTable (
        "aggregate_id" BYTEA PRIMARY KEY,
        "data" BYTEA,
        "version" INTEGER
      )
    |]
