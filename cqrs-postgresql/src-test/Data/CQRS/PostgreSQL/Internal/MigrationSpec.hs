{-# LANGUAGE OverloadedStrings #-}
module Data.CQRS.PostgreSQL.Internal.MigrationSpec
    ( mkApplyMigrationsSpec
    ) where

import           Control.Exception (bracket)
import           Control.Monad.IO.Class (liftIO)
import           Data.CQRS.PostgreSQL.Internal.Utils (runQuery, runTransactionP)
import           Data.CQRS.PostgreSQL.Internal.Migration (applyMigrations)
import           Data.Pool (Pool, withResource, destroyAllResources)
import           Data.Text (Text)
import           Database.PostgreSQL.LibPQ (Connection)
import           Test.Hspec

-- Tests for Data.CQRS.PostgreSQL.Internal.Migration
mkApplyMigrationsSpec :: IO (Pool Connection) -> Spec
mkApplyMigrationsSpec mkConnectionPool = do
  describe "applyMigrations" $ do
    it "can apply a single migration" $ withConnectionPool $ \connectionPool -> do
      -- Apply the migration
      applyMigrations' connectionPool
        [ (cid0, createXSql)
        ]
      -- Do a query which would fail without the migration
      assertValidQuery connectionPool selectFromX

    it "ignores migrations that have already been applied (single call)" $ withConnectionPool $ \connectionPool -> do
      -- Apply the migrations
      applyMigrations' connectionPool
        [ (cid0, createXSql)
        , (cid0, createXSql) -- Would fail if applied
        ]
      -- Do a query which would fail without at least one of the migrations being applied
      assertValidQuery connectionPool selectFromX

    it "ignores migrations that have already been applied (multiple calls)" $ withConnectionPool $ \connectionPool -> do
      -- Apply first migration
      applyMigrations' connectionPool
        [ (cid0, createXSql)
        ]
      -- Apply second migration
      applyMigrations' connectionPool
        [ (cid0, createXSql)
        ]
      -- Do a query which would fail without at least one of the migrations being applied
      assertValidQuery connectionPool selectFromX

    it "throws an error if SQL is changed for a given change set ID" $ withConnectionPool $ \connectionPool -> do
      -- Apply first migration
      applyMigrations' connectionPool
        [ (cid0, createXSql)
        ]
      -- Apply second migration, which SHOULD fail because the SQL is different
      applyMigrations' connectionPool
        [ (cid0, createXSqlBad)
        ] `shouldThrow` anyException

    it "can apply multiple distinct migrations in a single call" $ withConnectionPool $ \connectionPool -> do
      -- Apply both migrations
      applyMigrations' connectionPool
        [ (cid0, createXSql)
        , (cid1, createYSql)
        ]
      -- Do a query which would fail without at least one of the migrations being applied
      assertValidQuery connectionPool joinXY

  where
    withConnectionPool spec =
      bracket mkConnectionPool destroyAllResources spec

    assertValidQuery connectionPool sql = do
      runTransactionP connectionPool $ do
        rows <- runQuery sql []
        liftIO $ rows `shouldSatisfy` (\rs -> length rs >= 0) -- Don't care about size of result, just that query succeeded

    createXSql = "CREATE TABLE X (A INT)"
    createYSql = "CREATE TABLE Y (B INT)"
    createXSqlBad = "CREATE TABLE X (Y CHAR(1))"

    selectFromX = "SELECT * FROM X"
    joinXY = "SELECT * FROM X, Y where X.A = Y.B"

    cid0 = "a328156d-9875-4471-8192-0c86959badb3"
    cid1 = "00c6159c-c7f6-4cec-b63f-f70c1c4c7bb1"

applyMigrations' :: Pool Connection -> [(Text, Text)] -> IO ()
applyMigrations' connectionPool migrations = do
  withResource connectionPool $ \connection ->
      applyMigrations connection migrations
