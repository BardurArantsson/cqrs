{-# LANGUAGE OverloadedStrings #-}
module Data.CQRS.PostgreSQL.Internal.Migration
    ( applyMigrations
    , uuid
    ) where

import           Control.Monad (forM_)
import           Control.Monad.IO.Class (liftIO)
import           Data.ByteString (ByteString)
import           Data.CQRS.PostgreSQL.Internal.Utils (badQueryResultMsg, execSql, query, runTransaction, SqlValue(..))
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
  runTransaction c $ execSql sqlCreateChangeSetTbl [ ]
  -- Apply all the migrations.
  forM_ migrations $ \(changeSetId, sql) -> do
    let changeSetIdSql = SqlUUID $ Just changeSetId
    let sqlText = decodeUtf8 sql
    runTransaction c $ do
      -- Check if change set has already been applied
      existingChangeSet <- query sqlFindChangeSet [ changeSetIdSql ] $
        liftIO . extractChangeSet changeSetId
      case existingChangeSet of
        Just (_, sqlText') | sqlText == sqlText' ->
          return () -- Already applied, do nothing
        Just _ ->
          -- Applied, but SQL doesn't match. That's a huge problem, so we'll error out.
          error $ "Migration error: Changeset SQL modified: UUID " ++ show changeSetId
        Nothing -> do
          execSql sqlInsertChangeSet [ changeSetIdSql, SqlText $ Just sqlText ]
          execSql sql [ ]

  where

    extractChangeSet changeSetId i = SC.map (unpackChangeSet changeSetId) i >>= Streams.read

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
