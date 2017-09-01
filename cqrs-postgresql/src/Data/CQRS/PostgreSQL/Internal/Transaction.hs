{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.CQRS.PostgreSQL.Internal.Transaction
       ( runTransaction
       , runTransactionP
       ) where

import           Control.Exception.Lifted (throwIO)
import           Control.Exception.Enclosed (catchAny)
import           Control.Monad (void)
import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Trans.Control (MonadBaseControl)
import           Control.Exception (SomeException)
import           Data.CQRS.PostgreSQL.Internal.Query
import           Data.Pool (Pool, withResource)
import           Database.PostgreSQL.LibPQ (Connection)

-- | Run query in a transaction.
runTransaction :: forall a m . (MonadIO m, MonadBaseControl IO m) => Connection -> QueryT m a -> m a
runTransaction connection q = do
  begin
  catchAny runAction tryRollback
  where
    runAction :: m a
    runAction = do
      r <- unsafeRunQueryT connection q
      commit
      return r

    tryRollback :: SomeException -> m a
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
runTransactionP :: forall a m . (MonadIO m, MonadBaseControl IO m) => Pool Connection -> QueryT m a -> m a
runTransactionP pool action = withResource pool $ flip runTransaction action
