{-# LANGUAGE OverloadedStrings #-}
module Data.CQRS.PostgreSQL.Internal.QuerySpec
    ( mkQuerySpec
    ) where

import           Control.Exception (bracket)
import           Control.Monad.IO.Class (liftIO)
import           Data.ByteString (isInfixOf)
import           Data.CQRS.PostgreSQL.Internal.Query
import           Data.CQRS.PostgreSQL.Internal.Transaction
import           Data.CQRS.PostgreSQL.Internal.QueryError
import           Data.Pool (Pool, destroyAllResources)
import           Database.PostgreSQL.LibPQ (Connection)
import           Test.Hspec

-- Tests for Data.CQRS.PostgreSQL.Internal.Utils
mkQuerySpec :: IO (Pool Connection) -> Spec
mkQuerySpec mkConnectionPool = do
  describe "single-result query" $ do
    it "produces a result (non-parametric)" $ withConnectionPool $ do
      x <- queryAll "SELECT TRUE" []
      liftIO $ length x `shouldBe` 1
    it "produces a result (parametric)" $ withConnectionPool $ do
      x <- queryAll "SELECT TRUE WHERE $1" [SqlBool $ Just True]
      liftIO $ length x `shouldBe` 1
    it "produces no results if unsatisfiable" $ withConnectionPool $ do
      x <- queryAll "SELECT TRUE WHERE FALSE" []
      liftIO $ length x `shouldBe` 0
    it "throws a QueryError when invalid SQL statement is executed" $
      withConnectionPool (queryAll "MY BAD QUERY" []) `shouldThrow` (\e ->
        case e of
          QueryError (Just "42601") "PGRES_FATAL_ERROR" (Just msg) | "MY BAD QUERY" `isInfixOf` msg ->
            True
          _ ->
            False)
  describe "multi-result query" $
    it "produces the correct number of rows" $ withConnectionPool $ do
      x <- queryAll "SELECT GENERATE_SERIES(1,5)" []
      liftIO $ length x `shouldBe` 5
  describe "query1" $ do
    it "returns 'Nothing' for unsatisfiable query" $ withConnectionPool $ do
      x <- query1 "SELECT TRUE WHERE FALSE" []
      liftIO $ x `shouldBe` Nothing
    it "returns 'Just x' for a satisfiable query" $ withConnectionPool $ do
      x <- query1 "SELECT TRUE" []
      liftIO $ x `shouldBe` Just [SqlBool $ Just True]
    it "returns first row for multi-row query result" $ withConnectionPool $ do
      x <- query1 "SELECT GENERATE_SERIES(1 :: Int,5)" []
      liftIO $ x `shouldBe` Just [SqlInt32 $ Just 1]

  where
    withConnectionPool test =
      bracket mkConnectionPool destroyAllResources $
        flip runTransactionP test
