{-# LANGUAGE OverloadedStrings #-}
module Data.CQRS.PostgreSQL.Migrations
    ( Schema(..) -- Re-export
    , migrate
    ) where

import qualified Database.Peregrin as P
import           Database.Peregrin (QP(..))
import           Database.Peregrin.Metadata (Schema(..))
import           Database.PostgreSQL.Simple (Connection, Only(..))
import           Data.CQRS.PostgreSQL.Internal.Identifiers

-- | Apply all the necessary migrations to use any subset of the CQRS
-- functionality.
migrate :: Connection -> Schema -> IO ()
migrate connection schema =
  P.migrate connection schema
    [ ("d0bf19c5-6bca-4bb0-8dd8-8e1340ec7503", sqlCreateEventTbl, QP $ Only eventTable)
    , ("1bbd4b4c-db0c-445c-90dd-19f7b725cac4", sqlCreateLogicalTimestampIdx, QP $ Only eventTable)
    , ("c7258b1c-bc3c-467a-bc1f-f63ad330651f", sqlCreateSnapshotTbl, QP $ Only snapshotTable)
    ]
  where
    ids = mkIdentifiers schema
    eventTable = tblEvent ids
    snapshotTable = tblSnapshot ids

    sqlCreateEventTbl =
       "CREATE TABLE ? ( \
       \  \"aggregate_id\" BYTEA NOT NULL, \
       \  \"event_data\" BYTEA NOT NULL, \
       \  \"seq_no\" INTEGER NOT NULL, \
       \  \"timestamp\" BIGINT NOT NULL, \
       \  \"l_timestamp\" BIGSERIAL NOT NULL UNIQUE, \
       \  PRIMARY KEY (\"aggregate_id\", \"seq_no\") \
       \)"

    sqlCreateLogicalTimestampIdx =
       "CREATE INDEX \"l_timestamp\" ON ? ( \
       \  \"l_timestamp\" \
       \)"

    sqlCreateSnapshotTbl =
       "CREATE TABLE ? ( \
       \  \"aggregate_id\" BYTEA PRIMARY KEY, \
       \  \"data\" BYTEA, \
       \  \"version\" INTEGER \
       \)"
