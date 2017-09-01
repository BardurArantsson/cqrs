{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.CQRS.PostgreSQL.Internal.MigrationSpec
    ( mkApplyMigrationsSpec
    ) where

import           Control.Exception (bracket)
import           Control.Monad (forM_, void)
import           Data.CQRS.PostgreSQL.Internal.Migration
import           Data.CQRS.PostgreSQL.Internal.Query
import           Data.CQRS.PostgreSQL.Internal.Transaction
import           Data.CQRS.PostgreSQL.Metadata
import           Data.Pool (Pool, withResource, destroyAllResources)
import           Data.Text (Text)
import           Database.PostgreSQL.LibPQ (Connection)
import           Test.Hspec

-- Tests for Data.CQRS.PostgreSQL.Internal.Migration
mkApplyMigrationsSpec :: IO (Pool Connection) -> Spec
mkApplyMigrationsSpec mkConnectionPool =
  forM_ [DefaultSchema, NamedSchema "foobar"] $ \schema -> do
    -- Show which case we're in
    let extra = case schema of
                  DefaultSchema -> " (default schema)"
                  NamedSchema _ -> " (named schema)"
    -- Specs:
    describe ("applyMigrations" ++ extra) $ do
      it "can apply a single migration" $ withConnectionPool $ \connectionPool -> do
        -- Apply the migration
        applyMigrations' schema connectionPool
          [ (cid0, createXSql)
          ]
        -- Do a query which would fail without the migration
        assertValidQuery connectionPool selectFromX

      it "ignores migrations that have already been applied (single call)" $ withConnectionPool $ \connectionPool -> do
        -- Apply the migrations
        applyMigrations' schema connectionPool
          [ (cid0, createXSql)
          , (cid0, createXSql) -- Would fail if applied
          ]
        -- Do a query which would fail without at least one of the migrations being applied
        assertValidQuery connectionPool selectFromX

      it "ignores migrations that have already been applied (multiple calls)" $ withConnectionPool $ \connectionPool -> do
        -- Apply first migration
        applyMigrations' schema connectionPool
          [ (cid0, createXSql)
          ]
        -- Apply second migration
        applyMigrations' schema connectionPool
          [ (cid0, createXSql)
          ]
        -- Do a query which would fail without at least one of the migrations being applied
        assertValidQuery connectionPool selectFromX

      it "throws an error if SQL is changed for a given change set ID" $ withConnectionPool $ \connectionPool -> do
        -- Apply first migration
        applyMigrations' schema connectionPool
          [ (cid0, createXSql)
          ]
        -- Apply second migration, which SHOULD fail because the SQL is different
        applyMigrations' schema connectionPool
          [ (cid0, createXSqlBad)
          ] `shouldThrow` anyException

      it "can apply multiple distinct migrations in a single call" $ withConnectionPool $ \connectionPool -> do
        -- Apply both migrations
        applyMigrations' schema connectionPool
          [ (cid0, createXSql)
          , (cid1, createYSql)
          ]
        -- Do a query which would fail without at least one of the migrations being applied
        assertValidQuery connectionPool joinXY

  where
    withConnectionPool =
      bracket mkConnectionPool destroyAllResources

    assertValidQuery connectionPool sql =
      runTransactionP connectionPool $
        void $ queryAll sql []

    createXSql = "CREATE TABLE X (A INT)"
    createYSql = "CREATE TABLE Y (B INT)"
    createXSqlBad = "CREATE TABLE X (Y CHAR(1))"

    selectFromX = "SELECT * FROM X"
    joinXY = "SELECT * FROM X, Y where X.A = Y.B"

    cid0 = "a328156d-9875-4471-8192-0c86959badb3"
    cid1 = "00c6159c-c7f6-4cec-b63f-f70c1c4c7bb1"

applyMigrations' :: Schema -> Pool Connection -> [(Text, Text)] -> IO ()
applyMigrations' schema connectionPool migrations =
  withResource connectionPool $ \connection ->
      applyMigrations connection schema migrations
