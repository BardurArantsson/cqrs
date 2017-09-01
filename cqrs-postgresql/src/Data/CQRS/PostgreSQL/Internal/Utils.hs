{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.CQRS.PostgreSQL.Internal.Utils
       ( SqlValue(..) -- Re-export
       , Transaction
       , TransactionT
       , badQueryResultMsg
       , execSql
       , execSql'
       , query
       , query1
       , queryAll
       , runTransaction
       , runTransactionP
       ) where

import           Control.Exception.Lifted (throwIO)
import           Control.Exception.Enclosed (catchAny)
import           Control.Monad (forM, void)
import           Control.Monad.IO.Class (liftIO, MonadIO)
import           Control.Monad.Trans.Class (lift, MonadTrans(..))
import           Control.Monad.Trans.Control (MonadBaseControl)
import           Control.Monad.Trans.Reader (ReaderT, runReaderT, ask)
import           Control.Exception (SomeException)
import           Data.ByteString.Lex.Integral (readDecimal)
import           Data.CQRS.PostgreSQL.Internal.SqlValue
import           Data.CQRS.PostgreSQL.Internal.QueryError
import           Data.Pool (Pool, withResource)
import           Data.Text (Text)
import           Data.Text.Encoding (encodeUtf8)
import           Database.PostgreSQL.LibPQ (Connection, Format(..), ExecStatus(..), Column(..), Row(..), FieldCode(..))
import qualified Database.PostgreSQL.LibPQ as LP
import qualified System.IO.Streams as Streams
import           System.IO.Streams (InputStream)
import qualified System.IO.Streams.Combinators as SC
import qualified System.IO.Streams.List as SL

-- | Transaction monad transformer.
newtype TransactionT m a = TransactionT (ReaderT Connection m a)
  deriving (Functor, Applicative, Monad)

instance MonadTrans TransactionT where
  lift m = TransactionT $ lift m

instance MonadIO m => MonadIO (TransactionT m) where
  liftIO m = TransactionT $ liftIO m

-- | Transaction in PostgreSQL.
type Transaction a = TransactionT IO a

-- | Run transaction
runTransaction :: forall a m . (MonadIO m, MonadBaseControl IO m) => Connection -> TransactionT m a -> m a
runTransaction connection transaction = do
  begin
  catchAny runAction tryRollback
  where
    runAction :: m a
    runAction = do
      r <- unsafeRunTransaction connection transaction
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

    begin = execSqlImpl connection "START TRANSACTION;" [ ]
    commit = execSqlImpl connection "COMMIT TRANSACTION;" [ ]
    rollback = execSqlImpl connection "ROLLBACK TRANSACTION;" [ ]

-- | Perform the actions inside a Transaction on a connection WITHOUT
-- wrapping in any TRANSACTION statements.
unsafeRunTransaction :: Connection -> TransactionT m a -> m a
unsafeRunTransaction connection (TransactionT t) =
  runReaderT t connection

-- | Run a transaction with a connection from the given resource pool
-- and return the connection when the transaction ends.
runTransactionP :: forall a m . (MonadIO m, MonadBaseControl IO m) => Pool Connection -> TransactionT m a -> m a
runTransactionP pool action = withResource pool $ flip runTransaction action

-- | Execute a query with no result.
execSql :: (MonadIO m, MonadBaseControl IO m) => Text -> [SqlValue] -> TransactionT m ()
execSql sql parameters = void $ execSql' sql parameters

-- | Execute a query an return the number of updated rows (if available).
execSql' :: (MonadIO m, MonadBaseControl IO m) => Text -> [SqlValue] -> TransactionT m (Maybe Int)
execSql' sql parameters = query' sql parameters (\n _ -> return n)

-- | Run a query and fold over the results. The action receives an
-- 'InputStream' over all the rows in the result.
query :: (MonadIO m, MonadBaseControl IO m) => Text -> [SqlValue] -> (InputStream [SqlValue] -> TransactionT m a) -> TransactionT m a
query sql parameters f = query' sql parameters $ \_ is -> f is

query' :: (MonadIO m, MonadBaseControl IO m) => Text -> [SqlValue] -> (Maybe Int -> InputStream [SqlValue] -> TransactionT m a) -> TransactionT m a
query' sql parameters f = TransactionT $ do
  connection <- ask
  lift $ queryImpl connection sql parameters (\n is -> unsafeRunTransaction connection (f n is))

-- | Run a quest which is expected to return at most one result. Any
-- result rows past the first will be __ignored__.
query1 :: (MonadIO m, MonadBaseControl IO m) => Text -> [SqlValue] -> TransactionT m (Maybe [SqlValue])
query1 sql parameters =
  query sql parameters (liftIO . Streams.read)

execSqlImpl :: (MonadIO m, MonadBaseControl IO m) => Connection -> Text -> [SqlValue] -> m ()
execSqlImpl connection sql parameters =
  queryImpl connection sql parameters (\_ _ -> pure ())

queryImpl :: forall m a . (MonadIO m, MonadBaseControl IO m) => Connection -> Text -> [SqlValue] -> (Maybe Int -> InputStream [SqlValue] -> m a) -> m a
queryImpl connection sql parameters f = do
  -- Run the query
  r <- liftIO open
  -- Check the status
  status <- liftIO $ LP.resultStatus r
  if isOk status
    then do
      -- How many rows affected?
      cmdTuples <- liftIO $ LP.cmdTuples r
      n <- case cmdTuples of
        Nothing -> return Nothing
        Just x -> return $ fst <$> readDecimal x
      -- Create the input stream and feed it to 'f'
      makeInputStream r >>= f n
    else do
      -- Throw exception
      sqlState <- liftIO $ LP.resultErrorField r DiagSqlstate
      statusMessage <- liftIO $ LP.resStatus status
      errorMessage <- liftIO $ LP.resultErrorMessage r
      throwIO QueryError { qeSqlState = sqlState
                         , qeStatusMessage = statusMessage
                         , qeErrorMessage = errorMessage
                         }

  where
    isOk CommandOk = True
    isOk TuplesOk  = True
    isOk _         = False

    open = do
      parameters' <- forM parameters $ fromSqlValue connection
      mr <- LP.execParams connection (encodeUtf8 sql) parameters' Text
      case mr of
        Nothing -> error "No result set; something is very wrong"
        Just r -> return r

    makeInputStream :: LP.Result -> m (InputStream [SqlValue])
    makeInputStream r = do
      Col nFields <- liftIO $ LP.nfields r
      Row nRows <- liftIO $ LP.ntuples r
      let columns = map LP.toColumn [0.. nFields - 1]
      let loop i = if i >= nRows
                     then
                       return Nothing
                     else do
                       columnValues <- forM columns $ getSqlVal r $ LP.toRow i
                       return $ Just (columnValues, i + 1)
      liftIO $ SC.unfoldM loop 0

    getSqlVal r row c = do
      mval <- LP.getvalue' r row c
      typ <- LP.ftype r c
      toSqlValue (typ, mval)

-- Run a query and return a list of the rows in the result. __This will read
-- ALL rows in the result into memory. It is ONLY meant for testing unless
-- you're ABSOLUTELY SURE that you won't end up using too much memory!__
queryAll :: (MonadIO m, MonadBaseControl IO m) => Text -> [SqlValue] -> TransactionT m [[SqlValue]]
queryAll sql parameters = query sql parameters (liftIO . SL.toList)

-- | Format a message indicating a bad query result due to the "shape".
badQueryResultMsg :: [String] -> [SqlValue] -> String
badQueryResultMsg params columns = concat ["Invalid query result shape. Params: ", show params, ". Result columns: ", show columns]
