{-# LANGUAGE OverloadedStrings #-}
module Data.CQRS.PostgreSQL.Internal.UtilsSpec
    ( mkUtilsSpec
    ) where

import           Control.Exception (bracket)
import           Data.ByteString (isInfixOf)
import           Data.CQRS.PostgreSQL.Internal.Utils (SqlValue(..), QueryError(..), runQuery)
import           Data.Pool (Pool, destroyAllResources)
import           Database.PostgreSQL.LibPQ (Connection)
import           Test.Hspec

-- Tests for Data.CQRS.PostgreSQL.Internal.Utils
mkUtilsSpec :: IO (Pool Connection) -> Spec
mkUtilsSpec mkConnectionPool = do
  describe "single-result query" $ do
    it "produces a result (non-parametric)" $ withConnectionPool $ \connectionPool -> do
      x <- runQuery connectionPool "SELECT TRUE" []
      length x `shouldBe` 1
    it "produces a result (parametric)" $ withConnectionPool $ \connectionPool -> do
      x <- runQuery connectionPool "SELECT TRUE WHERE $1" [SqlBool $ Just True]
      length x `shouldBe` 1
    it "produces no results if unsatisfiable" $ withConnectionPool $ \connectionPool -> do
      x <- runQuery connectionPool "SELECT TRUE WHERE FALSE" []
      length x `shouldBe` 0
    it "throws a QueryError when invalid SQL statement is executed" $ withConnectionPool $ \connectionPool -> do
      runQuery connectionPool "MY BAD QUERY" [] `shouldThrow` (\e ->
        case e of
          QueryError (Just "42601") "PGRES_FATAL_ERROR" (Just msg) | "MY BAD QUERY" `isInfixOf` msg ->
            True
          _ ->
            False)
  describe "multi-result query" $ do
    it "produces the correct number of rows" $ withConnectionPool $ \connectionPool -> do
      x <- runQuery connectionPool "SELECT GENERATE_SERIES(1,5)" []
      length x `shouldBe` 5
  where
    withConnectionPool test =
      bracket mkConnectionPool destroyAllResources test
