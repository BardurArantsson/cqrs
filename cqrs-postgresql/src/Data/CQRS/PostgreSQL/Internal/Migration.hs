{-# LANGUAGE OverloadedStrings #-}
module Data.CQRS.PostgreSQL.Internal.Migration
    ( applyMigrations
    ) where

import           Control.Monad (forM_)
import           Control.Monad.IO.Class (liftIO)
import           Data.CQRS.PostgreSQL.Internal.Utils (badQueryResultMsg, execSql, query, runTransaction, SqlValue(..))
import           Data.Text (Text)
import           Database.PostgreSQL.LibPQ (Connection)
import qualified System.IO.Streams as Streams
import qualified System.IO.Streams.Combinators as SC

-- | Apply all necessary migrations to event store database.
-- We use a special table to track which migrations have been
-- applied.
applyMigrations :: Connection -> [(Text, Text)] -> IO ()
applyMigrations c migrations = do
  -- Must always create the change log if necessary
  runTransaction c $ execSql sqlCreateChangeSetTbl [ ]
  -- Apply all the migrations.
  forM_ migrations $ \(changeSetId, sql) -> do
    let changeSetIdSql = SqlText $ Just changeSetId
    runTransaction c $ do
      -- Check if change set has already been applied
      existingChangeSet <- query sqlFindChangeSet [ changeSetIdSql ] $
        liftIO . extractChangeSet changeSetId
      case existingChangeSet of
        Just (_, sql') | sql == sql' ->
          return () -- Already applied, do nothing
        Just _ ->
          -- Applied, but SQL doesn't match. That's a huge problem, so we'll error out.
          error $ "Migration error: Changeset SQL modified: " ++ show changeSetId
        Nothing -> do
          execSql sqlInsertChangeSet [ changeSetIdSql, SqlText $ Just sql ]
          execSql sql [ ]

  where

    extractChangeSet changeSetId i = SC.map (unpackChangeSet changeSetId) i >>= Streams.read

    unpackChangeSet _ [ SqlText (Just changeSetId), SqlText (Just sqlText) ] = (changeSetId, sqlText)
    unpackChangeSet changeSetId columns = error $ badQueryResultMsg [show changeSetId] columns

    -- Migrations support SQL:
    sqlCreateChangeSetTbl =
        "CREATE TABLE IF NOT EXISTS changeset ( \
         \  changeset_id TEXT PRIMARY KEY, \
         \  sql TEXT NOT NULL \
         \)"
    sqlFindChangeSet =
        "SELECT changeset_id, sql FROM changeset \
        \ WHERE changeset_id = $1"
    sqlInsertChangeSet =
        "INSERT INTO changeset (changeset_id, sql) \
        \ VALUES ($1, $2)"
