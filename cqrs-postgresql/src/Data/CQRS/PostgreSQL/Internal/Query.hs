{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.CQRS.PostgreSQL.Internal.Query
       ( QueryT
       , SqlValue(..) -- Re-export
       , badQueryResultMsg
       , execute
       , execute'
       , query
       , query1
       , queryAll
       , unsafeExecute
       , unsafeRunQueryT
       ) where

import           Control.Exception.Lifted (throwIO)
import           Control.Monad (forM, void)
import           Control.Monad.IO.Class (liftIO, MonadIO)
import           Control.Monad.Trans.Class (lift, MonadTrans(..))
import           Control.Monad.Trans.Control (MonadBaseControl)
import           Control.Monad.Trans.Reader (ReaderT, runReaderT, ask)
import           Data.ByteString.Lex.Integral (readDecimal)
import           Data.CQRS.PostgreSQL.Internal.SqlValue
import           Data.CQRS.PostgreSQL.Internal.QueryError
import           Data.Text (Text)
import           Data.Text.Encoding (encodeUtf8)
import           Database.PostgreSQL.LibPQ (Connection, Format(..), ExecStatus(..), Column(..), Row(..), FieldCode(..))
import qualified Database.PostgreSQL.LibPQ as LP
import qualified System.IO.Streams as Streams
import           System.IO.Streams (InputStream)
import qualified System.IO.Streams.Combinators as SC
import qualified System.IO.Streams.List as SL

-- | Query monad transformer.
newtype QueryT m a = QueryT { unQueryT :: ReaderT Connection m a }
  deriving (Functor, Applicative, Monad)

instance MonadTrans QueryT where
  lift m = QueryT $ lift m

instance MonadIO m => MonadIO (QueryT m) where
  liftIO m = QueryT $ liftIO m

-- | Run query on a given 'Connection'. Performs no transaction
-- control.
unsafeRunQueryT :: Connection -> QueryT m a -> m a
unsafeRunQueryT c (QueryT q) = runReaderT q c

-- | Execute a query with no result.
execute :: (MonadIO m, MonadBaseControl IO m) => Text -> [SqlValue] -> QueryT m ()
execute sql parameters = void $ execute' sql parameters

-- | Execute a query an return the number of updated rows (if available).
execute' :: (MonadIO m, MonadBaseControl IO m) => Text -> [SqlValue] -> QueryT m (Maybe Int)
execute' sql parameters = QueryT $ do
  connection <- ask
  unsafeExecute connection sql parameters

-- | Run a query and fold over the results. The action receives an
-- 'InputStream' over all the rows in the result.
query :: (MonadIO m, MonadBaseControl IO m) => Text -> [SqlValue] -> (InputStream [SqlValue] -> QueryT m a) -> QueryT m a
query sql parameters f = QueryT $ do
  connection <- ask
  lift $ unsafeQueryOrUpdate connection sql parameters
           (\_ is -> runReaderT (unQueryT $ f is) connection)

-- | Run a quest which is expected to return at most one result. Any
-- result rows past the first will be __ignored__.
query1 :: (MonadIO m, MonadBaseControl IO m) => Text -> [SqlValue] -> QueryT m (Maybe [SqlValue])
query1 sql parameters =
  query sql parameters (liftIO . Streams.read)

-- | Run a query and return a list of the rows in the result. __This will read
-- ALL rows in the result into memory. It is ONLY meant for testing unless
-- you're ABSOLUTELY SURE that you won't end up using too much memory!__
queryAll :: (MonadIO m, MonadBaseControl IO m) => Text -> [SqlValue] -> QueryT m [[SqlValue]]
queryAll sql parameters =
  query sql parameters (liftIO . SL.toList)

-- | Execute query, ignoring the result.
unsafeExecute :: (MonadIO m, MonadBaseControl IO m) => Connection -> Text -> [SqlValue] -> m (Maybe Int)
unsafeExecute connection sql parameters =
  unsafeQueryOrUpdate connection sql parameters (\n _ -> pure n)

-- | Execute query.
unsafeQueryOrUpdate :: forall m a . (MonadIO m, MonadBaseControl IO m) => Connection -> Text -> [SqlValue] -> (Maybe Int -> InputStream [SqlValue] -> m a) -> m a
unsafeQueryOrUpdate connection sql parameters f = do
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

-- | Format a message indicating a bad query result due to the "shape".
badQueryResultMsg :: [String] -> [SqlValue] -> String
badQueryResultMsg params columns = concat ["Invalid query result shape. Params: ", show params, ". Result columns: ", show columns]
