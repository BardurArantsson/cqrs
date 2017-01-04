{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Data.CQRS.PostgreSQL.Internal.Migration
    ( applyMigrations
    ) where

import           Control.Monad (forM_, when)
import           Data.CQRS.PostgreSQL.Internal.Utils
import           Data.CQRS.PostgreSQL.Metadata
import           Data.Text (Text)
import           Data.Int (Int32)
import           Database.PostgreSQL.LibPQ (Connection)
import           NeatInterpolation (text)

-- | Apply a list of migrations to a database. Any
-- migrations that have already been applied (as identified) by the
-- identifier of the migration (i.e. the first element of the pair)
-- will be skipped. Unless the 'Connection' is set up beforehand, all
-- migrations have to specify their schema explicitly in the SQL.
--
-- Migrations are tracked using two tables, namely "@__migration@" and
-- "@__migration_meta@", which will be created in the given
-- 'schema'.
applyMigrations :: Connection -> Schema -> [(Text, Text)] -> IO ()
applyMigrations c schema migrations = do
  -- Must always create the "migration_meta" table (and its
  -- schema) if necessary. Having just created this table without
  -- any rows represents "version 0" of the metadata data
  -- structures.
  forM_ sqlCreateSchema $ \s -> runTransaction c $ execSql s [ ]
  runTransaction c $ execSql sqlCreateMetaTbl [ ]
  -- Apply meta-migrations.
  withLock $ do
    -- Apply meta-migrations; these are hardcoded for obvious reasons.
    -- EXCEPT for the very first migration, NO changes may be made to
    -- the "migration_meta" table in any migration here. This is to
    -- ensure 'perpetual' compatibility.
    metaMigrate 1 [ sqlInsertMetaVersion0
                  , sqlCreateMigrationTbl
                  ]
  -- Apply all the migrations.
  forM_ migrations $ \(changeSetId, sql) -> do
    let changeSetIdSql = SqlText $ Just changeSetId
    withLock $ do
      -- Check if change set has already been applied
      existingMigration <- query1 sqlFindMigration [ changeSetIdSql ]
      case fmap (unpackMigration changeSetId) existingMigration of
        Just (_, sql') | sql == sql' ->
          return () -- Already applied, do nothing
        Just _ ->
          -- Applied, but SQL doesn't match. That's a huge problem, so we'll error out.
          error $ "Migration error: Changeset SQL modified: " ++ show changeSetId
        Nothing -> do
          execSql sqlInsertMigration [ changeSetIdSql, SqlText $ Just sql ]
          execSql sql [ ]

  where

    unpackMigration _ [ SqlText (Just changeSetId), SqlText (Just sqlText) ] = (changeSetId, sqlText)
    unpackMigration changeSetId columns = error $ badQueryResultMsg [show changeSetId] columns

    unpackMetaVersion [ SqlInt32 (Just v) ] = v
    unpackMetaVersion columns = error $ badQueryResultMsg [] columns

    -- SQL identifiers
    migrationTableSql = toSQL $ Table schema "migration"
    migrationMetaTableSql = toSQL $ Table schema "migration_meta"
    migrationSchemaSql = case schema of
                           DefaultSchema -> Nothing
                           NamedSchema s -> Just (quoteToSQL s)

    -- Apply meta-migrations for the given base version number. The migration
    -- is skipped if it has been performed before.
    metaMigrate :: Int32 -> [Text] -> Transaction ()
    metaMigrate metaVersion sqls = do
      -- Get the meta-version; defaults to 0 if we've only just
      -- created the metadata table.
      currentMetaVersion <- fmap (maybe 0 id) . fmap (fmap unpackMetaVersion) $ query1 sqlGetMetaVersion [ ]
      -- If the migration is applicable, then we apply it.
      when (currentMetaVersion + 1 == metaVersion) $ do
        forM_ sqls $ \sql -> execSql sql []
        rowCount <- fmap (maybe 0 id) $ execSql' sqlUpdateMetaVersion [ SqlInt32 $ Just metaVersion
                                                                      , SqlInt32 $ Just currentMetaVersion
                                                                      ]
        when (rowCount /= 1) $ error $ "Unexpected row count " ++ show rowCount ++ " from update on \"migration_meta\" table!"

    -- Perform a transaction with the exclusive lock held. The lock is
    -- automatically released when the transaction ends.
    withLock :: Transaction a -> IO a
    withLock txn =
      runTransaction c $ do
        execSql sqlLockMetaTbl [ ]
        txn

    -- Migrations support SQL:
    sqlCreateMetaTbl = [text|
      CREATE TABLE IF NOT EXISTS $migrationMetaTableSql ("meta_version" INTEGER PRIMARY KEY)
    |]

    sqlCreateSchema = (flip fmap) migrationSchemaSql $ \s -> [text|
      CREATE SCHEMA IF NOT EXISTS $s
    |]

    sqlLockMetaTbl = [text|
      LOCK TABLE $migrationMetaTableSql IN ACCESS EXCLUSIVE MODE
    |]

    sqlGetMetaVersion = [text|
      SELECT "meta_version"
        FROM $migrationMetaTableSql
    |]

    sqlUpdateMetaVersion = [text|
      UPDATE $migrationMetaTableSql
         SET "meta_version" = $$1
       WHERE "meta_version" = $$2
    |]

    sqlInsertMetaVersion0 = [text|
      INSERT INTO $migrationMetaTableSql
                  ("meta_version")
           VALUES (0)
    |]

    sqlCreateMigrationTbl = [text|
      CREATE TABLE $migrationTableSql (
        "id" TEXT PRIMARY KEY,
        "sql" TEXT NOT NULL
      )
    |]

    sqlFindMigration = [text|
      SELECT "id", "sql"
        FROM $migrationTableSql
       WHERE "id" = $$1
    |]

    sqlInsertMigration = [text|
      INSERT INTO $migrationTableSql
                  ("id", "sql")
           VALUES ($$1, $$2)
    |]
