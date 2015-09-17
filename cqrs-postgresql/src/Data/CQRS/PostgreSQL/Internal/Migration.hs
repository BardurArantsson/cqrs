{-# LANGUAGE OverloadedStrings #-}
module Data.CQRS.PostgreSQL.Internal.Migration
    ( applyMigrations
    , uuid
    ) where

import           Control.Monad (forM_, (>=>))
import           Data.ByteString (ByteString)
import           Data.CQRS.PostgreSQL.Internal.Utils (badQueryResultMsg, execSql, ioQuery, withTransaction, SqlValue(..))
import           Data.UUID.Types (UUID)
import qualified Data.UUID.Types as U
import           Data.Text.Encoding (decodeUtf8)
import           Database.PostgreSQL.LibPQ (Connection)
import qualified System.IO.Streams as Streams
import qualified System.IO.Streams.Combinators as SC

-- | Convert valid UUID string to a value.
uuid :: ByteString -> UUID
uuid s =
  case U.fromASCIIBytes s of
    Nothing -> error $ "Invalid UUID string: " ++ show s
    Just u -> u

-- | Apply all necessary migrations to event store database.
-- We use a special table to track which migrations have been
-- applied.
applyMigrations :: Connection -> [(UUID, ByteString)] -> IO ()
applyMigrations c migrations = do
  -- Must always create the change log if necessary
  withTransaction c $ execSql c sqlCreateChangeSetTbl [ ]
  -- Apply all the migrations.
  forM_ migrations $ \(changeSetId, sql) -> do
    let changeSetIdSql = SqlUUID $ Just changeSetId
    let sqlText = decodeUtf8 sql
    withTransaction c $ do
      -- Check if change set has already been applied
      existingChangeSet <- ioQuery c sqlFindChangeSet [ changeSetIdSql ] $
                             SC.map (unpackChangeSet changeSetId) >=> Streams.read
      case existingChangeSet of
        Just (_, sqlText') | sqlText == sqlText' ->
          return () -- Already applied, do nothing
        Just _ ->
          -- Applied, but SQL doesn't match. That's a huge problem, so we'll error out.
          error $ "Migration error: Changeset SQL modified: UUID " ++ show changeSetId
        Nothing -> do
          execSql c sqlInsertChangeSet [ changeSetIdSql, SqlText $ Just sqlText ]
          execSql c sql [ ]

  where
    unpackChangeSet _ [ SqlUUID (Just changeSetId), SqlText (Just sqlText) ] = (changeSetId, sqlText)
    unpackChangeSet changeSetId columns = error $ badQueryResultMsg [show changeSetId] columns

    -- Migrations support SQL:
    sqlCreateChangeSetTbl =
        "CREATE TABLE IF NOT EXISTS changeset ( \
         \  changeset_id UUID PRIMARY KEY, \
         \  sql TEXT NOT NULL \
         \)"
    sqlFindChangeSet =
        "SELECT changeset_id, sql FROM changeset \
        \ WHERE changeset_id = $1"
    sqlInsertChangeSet =
        "INSERT INTO changeset (changeset_id, sql) \
        \ VALUES ($1, $2)"
