{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.CQRS.PostgreSQL.Internal.QuerySpec
    ( mkQuerySpec
    ) where

import           Control.Exception (bracket)
import           Control.Monad.IO.Class (liftIO)
import           Data.ByteString (isInfixOf)
import           Data.CQRS.PostgreSQL.Internal.Query
import           Data.CQRS.PostgreSQL.Internal.Transaction
import           Data.Int (Int32)
import           Data.Pool (Pool, destroyAllResources)
import           Database.PostgreSQL.Simple (Connection, Only(..), SqlError(..), ExecStatus(..))
import           Test.Hspec

-- Tests for Data.CQRS.PostgreSQL.Internal.Utils
mkQuerySpec :: IO (Pool Connection) -> Spec
mkQuerySpec mkConnectionPool = do
  describe "single-result query" $ do
    it "produces a result (non-parametric)" $ withConnectionPool $ do
      (x :: [Only Bool]) <- queryAll "SELECT TRUE" ()
      liftIO $ length x `shouldBe` 1
    it "produces a result (parametric)" $ withConnectionPool $ do
      (x :: [Only Bool]) <- queryAll "SELECT TRUE WHERE ?" (Only True)
      liftIO $ length x `shouldBe` 1
    it "produces no results if unsatisfiable" $ withConnectionPool $ do
      (x :: [Only Bool]) <- queryAll "SELECT TRUE WHERE FALSE" ()
      liftIO $ length x `shouldBe` 0
    it "throws a SqlError when invalid SQL statement is executed" $
      withConnectionPool (queryAll "MY BAD QUERY" () :: QueryT IO [Only Bool]) `shouldThrow` (\e ->
        case e of
          SqlError "42601" FatalError msg _ _ | "syntax error at or near \"MY" `isInfixOf` msg ->
            True
          _ ->
            False)
  describe "multi-result query" $
    it "produces the correct number of rows" $ withConnectionPool $ do
      (x :: [Only Int32]) <- queryAll "SELECT GENERATE_SERIES(1,5)" ()
      liftIO $ length x `shouldBe` 5
  describe "query1" $ do
    it "returns 'Nothing' for unsatisfiable query" $ withConnectionPool $ do
      (x :: Maybe (Only Bool)) <- query1 "SELECT TRUE WHERE FALSE" ()
      liftIO $ x `shouldBe` Nothing
    it "returns 'Just x' for a satisfiable query" $ withConnectionPool $ do
      (x :: Maybe (Only Bool)) <- query1 "SELECT TRUE" ()
      liftIO $ x `shouldBe` Just (Only True)
    it "returns first row for multi-row query result" $ withConnectionPool $ do
      (x :: Maybe (Only Int32)) <- query1 "SELECT GENERATE_SERIES(1 :: Int,5)" ()
      liftIO $ x `shouldBe` Just (Only 1)

  where
    withConnectionPool test =
      bracket mkConnectionPool destroyAllResources $
        flip runTransactionP test
