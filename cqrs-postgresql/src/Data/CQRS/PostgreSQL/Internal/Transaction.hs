{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.CQRS.PostgreSQL.Internal.Transaction
       ( runTransaction
       , runTransactionP
       ) where

import           Control.Exception (throwIO)
import           Control.Exception.Enclosed (catchAny)
import           Control.Monad (void)
import           Control.Exception (SomeException)
import           Data.CQRS.PostgreSQL.Internal.Query
import           Data.Pool (Pool, withResource)
import           Database.PostgreSQL.LibPQ (Connection)

-- | Run query in a transaction.
runTransaction :: forall a . Connection -> QueryT IO a -> IO a
runTransaction connection q = do
  begin
  catchAny runAction tryRollback
  where
    runAction :: IO a
    runAction = do
      r <- unsafeRunQueryT connection q
      commit
      return r

    tryRollback :: SomeException -> IO a
    tryRollback e =
      -- Try explicit rollback; we want to preserve original exception.
      catchAny (rollback >> throwIO e) $ \_ ->
          -- Rethrow original exception; resource pool will make sure the database
          -- connection is properly destroyed (rather than being returned to the
          -- pool in some indeterminate state).
          throwIO e

    tx sql = void $ unsafeExecute connection sql [ ]

    begin = tx "START TRANSACTION ISOLATION LEVEL REPEATABLE READ;"
    commit = tx "COMMIT TRANSACTION;"
    rollback = tx "ROLLBACK TRANSACTION;"

-- | Run a transaction with a connection from the given resource pool
-- and return the connection when the transaction ends.
runTransactionP :: Pool Connection -> QueryT IO a -> IO a
runTransactionP pool action = withResource pool $ flip runTransaction action
