{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.CQRS.PostgreSQL.Internal.Transaction
       ( runTransaction
       , runTransactionIO
       , runTransactionP
       , runTransactionPIO
       ) where

import           Control.Exception (SomeException)
import           Control.Monad (void)
import           Control.Monad.Trans.Class (lift)
import           Data.CQRS.PostgreSQL.Internal.Query
import           Database.PostgreSQL.Simple (Connection)
import           UnliftIO (MonadUnliftIO, catchAny, throwIO)
import           UnliftIO.Pool (Pool, withResource)

-- | Run query in a transaction.
runTransaction :: forall a m . (MonadUnliftIO m) => Connection -> QueryT m a -> m a
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

    tx sql = void $ unsafeExecute connection sql ()

    begin = tx "START TRANSACTION ISOLATION LEVEL REPEATABLE READ;"
    commit = tx "COMMIT TRANSACTION;"
    rollback = tx "ROLLBACK TRANSACTION;"

-- | Run a transaction with a connection from the given resource pool
-- and return the connection when the transaction ends.
runTransactionP :: (MonadUnliftIO m) => Pool Connection -> QueryT m a -> m a
runTransactionP pool action =
    withResource pool $ \c ->
      runTransaction c action

-- | Run an I/O action inside a transaction.
runTransactionIO :: (MonadUnliftIO m) => Connection -> m a -> m a
runTransactionIO connection f = do
  runTransaction connection $ QueryT $ lift f

-- | Run an I/O action inside a transaction on a connection from the
-- given pool.
runTransactionPIO :: (MonadUnliftIO m) => Pool Connection -> (Connection -> m a) -> m a
runTransactionPIO pool f =
  withResource pool $ \c -> runTransactionIO c (f c)
