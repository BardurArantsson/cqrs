{-# LANGUAGE OverloadedStrings #-}
module Data.CQRS.PostgreSQL.Internal.UtilsSpec
    ( mkUtilsSpec
    ) where

import           Control.Exception (bracket)
import           Control.Monad.IO.Class (liftIO)
import           Data.ByteString (isInfixOf)
import           Data.CQRS.PostgreSQL.Internal.Utils (SqlValue(..), QueryError(..), runQuery, runTransactionP)
import           Data.Pool (Pool, destroyAllResources)
import           Database.PostgreSQL.LibPQ (Connection)
import           Test.Hspec

-- Tests for Data.CQRS.PostgreSQL.Internal.Utils
mkUtilsSpec :: IO (Pool Connection) -> Spec
mkUtilsSpec mkConnectionPool = do
  describe "single-result query" $ do
    it "produces a result (non-parametric)" $ withConnectionPool $ do
      x <- runQuery "SELECT TRUE" []
      liftIO $ length x `shouldBe` 1
    it "produces a result (parametric)" $ withConnectionPool $ do
      x <- runQuery "SELECT TRUE WHERE $1" [SqlBool $ Just True]
      liftIO $ length x `shouldBe` 1
    it "produces no results if unsatisfiable" $ withConnectionPool $ do
      x <- runQuery "SELECT TRUE WHERE FALSE" []
      liftIO $ length x `shouldBe` 0
    it "throws a QueryError when invalid SQL statement is executed" $ do
      (withConnectionPool $ runQuery "MY BAD QUERY" []) `shouldThrow` (\e ->
        case e of
          QueryError (Just "42601") "PGRES_FATAL_ERROR" (Just msg) | "MY BAD QUERY" `isInfixOf` msg ->
            True
          _ ->
            False)
  describe "multi-result query" $ do
    it "produces the correct number of rows" $ withConnectionPool $ do
      x <- runQuery "SELECT GENERATE_SERIES(1,5)" []
      liftIO $ length x `shouldBe` 5
  where
    withConnectionPool test =
      bracket mkConnectionPool destroyAllResources $
        (flip runTransactionP) test
