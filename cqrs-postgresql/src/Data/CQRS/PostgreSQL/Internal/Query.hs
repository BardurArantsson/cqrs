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

import           Control.Exception (throwIO)
import           Control.Monad (forM, void)
import           Control.Monad.IO.Class (liftIO, MonadIO)
import           Control.Monad.Trans.Class (lift, MonadTrans(..))
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
newtype QueryT m a = QueryT (ReaderT Connection m a)
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
execute :: Text -> [SqlValue] -> QueryT IO ()
execute sql parameters = void $ execute' sql parameters

-- | Execute a query an return the number of updated rows (if available).
execute' :: Text -> [SqlValue] -> QueryT IO (Maybe Int)
execute' sql parameters = QueryT $ do
  connection <- ask
  liftIO $ unsafeExecute connection sql parameters

-- | Run a query and fold over the results. The action receives an
-- 'InputStream' over all the rows in the result.
query :: Text -> [SqlValue] -> (InputStream [SqlValue] -> QueryT IO a) -> QueryT IO a
query sql parameters f = QueryT $ do
  c <- ask
  liftIO $ unsafeQuery c sql parameters (unsafeRunQueryT c . f)

-- | Run a quest which is expected to return at most one result. Any
-- result rows past the first will be __ignored__.
query1 :: Text -> [SqlValue] -> QueryT IO (Maybe [SqlValue])
query1 sql parameters =
  query sql parameters (liftIO . Streams.read)

-- | Run a query and return a list of the rows in the result. __This will read
-- ALL rows in the result into memory. It is ONLY meant for testing unless
-- you're ABSOLUTELY SURE that you won't end up using too much memory!__
queryAll :: Text -> [SqlValue] -> QueryT IO [[SqlValue]]
queryAll sql parameters =
  query sql parameters (liftIO . SL.toList)

-- | Execute query, returning the number of updated rows.
unsafeExecute :: Connection -> Text -> [SqlValue] -> IO (Maybe Int)
unsafeExecute connection sql parameters =
  unsafeQueryOrUpdate connection sql parameters (\n _ -> pure n)

-- | Execute query.
unsafeQuery :: Connection -> Text -> [SqlValue] -> (InputStream [SqlValue] -> IO a) -> IO a
unsafeQuery connection sql parameters f = do
  unsafeQueryOrUpdate connection sql parameters (\_ is -> f is)

-- | Execute query.
unsafeQueryOrUpdate :: Connection -> Text -> [SqlValue] -> (Maybe Int -> InputStream [SqlValue] -> IO a) -> IO a
unsafeQueryOrUpdate connection sql parameters f = do
  -- Run the query
  r <- open
  -- Check the status
  status <- LP.resultStatus r
  if isOk status
    then do
      -- How many rows affected?
      cmdTuples <- LP.cmdTuples r
      n <- case cmdTuples of
        Nothing -> return Nothing
        Just x -> return $ fst <$> readDecimal x
      -- Create the input stream and feed it to 'f'
      makeInputStream r >>= f n
    else do
      -- Throw exception
      sqlState <- LP.resultErrorField r DiagSqlstate
      statusMessage <- LP.resStatus status
      errorMessage <- LP.resultErrorMessage r
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

    makeInputStream :: LP.Result -> IO (InputStream [SqlValue])
    makeInputStream r = do
      Col nFields <- LP.nfields r
      Row nRows <- LP.ntuples r
      let columns = map LP.toColumn [0.. nFields - 1]
      let loop i = if i >= nRows
                     then
                       return Nothing
                     else do
                       columnValues <- forM columns $ getSqlVal r $ LP.toRow i
                       return $ Just (columnValues, i + 1)
      SC.unfoldM loop 0

    getSqlVal r row c = do
      mval <- LP.getvalue' r row c
      typ <- LP.ftype r c
      toSqlValue (typ, mval)

-- | Format a message indicating a bad query result due to the "shape".
badQueryResultMsg :: [String] -> [SqlValue] -> String
badQueryResultMsg params columns = concat ["Invalid query result shape. Params: ", show params, ". Result columns: ", show columns]
