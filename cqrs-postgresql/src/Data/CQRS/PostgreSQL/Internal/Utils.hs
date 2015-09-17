{-# LANGUAGE BangPatterns, DeriveGeneric, OverloadedStrings #-}
module Data.CQRS.PostgreSQL.Internal.Utils
       ( SqlValue(..)
       , QueryError(..)
       , badQueryResultMsg
       , execSql
       , execSql'
       , ioQuery
       , ioQuery'
       , isDuplicateKey
       , runQuery
       , withTransaction
       ) where

import           Control.DeepSeq (NFData(..), ($!!))
import           Control.Exception (Exception, throw)
import           Control.Exception.Enclosed (catchAny)
import           Control.Monad (forM)
import           Control.Monad.IO.Class (liftIO)
import           Control.Exception (SomeException, bracket)
import qualified Data.ByteString.Char8 as B8
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import           Data.ByteString.Lex.Integral (readDecimal)
import           Data.ByteString.Lex.Double (readDouble)
import           Data.Int (Int16, Int32, Int64)
import           Data.Pool (Pool, withResource)
import           Data.Text (Text)
import           Data.Text.Encoding (decodeUtf8', encodeUtf8)
import           Data.Typeable (Typeable)
import           Data.UUID.Types (UUID)
import qualified Data.UUID.Types as U
import           Database.PostgreSQL.LibPQ (Connection, Oid(..), Format(..), ExecStatus(..), Column(..), Row(..), FieldCode(..))
import qualified Database.PostgreSQL.LibPQ as P
import           GHC.Generics (Generic)
import           System.IO.Streams (InputStream)
import qualified System.IO.Streams.Combinators as SC
import qualified System.IO.Streams.List as SL

-- | Known field types.
data SqlValue = SqlByteArray (Maybe ByteString)
              | SqlBlankPaddedString (Maybe ByteString)
              | SqlBool (Maybe Bool)
              | SqlInt16 (Maybe Int16)
              | SqlInt32 (Maybe Int32)
              | SqlInt64 (Maybe Int64)
              | SqlFloating (Maybe Double)
              | SqlVarChar (Maybe Text)
              | SqlText (Maybe Text)
              | SqlUUID (Maybe UUID)
              | Unmatched (Oid, Maybe ByteString)
              deriving (Eq, Show)

-- | Is the given query exception a duplicate key exception?
isDuplicateKey :: QueryError -> Maybe ()
isDuplicateKey qe | qeSqlState qe == Just "23505" = Just ()
                  | otherwise                     = Nothing

-- | Execute an IO action with an active transaction.
withTransaction :: Connection -> IO a -> IO a
withTransaction connection action = do
  begin
  catchAny runAction tryRollback
  where
    runAction = do
      r <- action
      commit
      return r

    tryRollback :: SomeException -> IO a
    tryRollback e =
      -- Try explicit rollback; we want to preserve original exception.
      catchAny (rollback >> throw e) $ \_ ->
          -- Rethrow original exception; resource pool will make sure the database
          -- connection is properly destroyed (rather than being returned to the
          -- pool).
          throw e

    begin = execSql connection "START TRANSACTION;" [ ]
    commit = execSql connection "COMMIT TRANSACTION;" [ ]
    rollback = execSql connection "ROLLBACK TRANSACTION;" [ ]

-- | Read a boolean.
readBoolean :: ByteString -> Maybe Bool
readBoolean "t" = Just True
readBoolean "f" = Just False
readBoolean _ = Nothing

-- | Map an SqlValue to a parameter.
fromSqlValue :: Connection -> SqlValue -> IO (Maybe (Oid, ByteString, Format))
fromSqlValue connection (SqlByteArray a) = do
  case a of
    Nothing -> return Nothing
    Just a' -> do
      x <- P.escapeByteaConn connection a'
      case x of
        Nothing -> error "Conversion failed"
        Just x' -> return $ Just (Oid 17, x', Text)
fromSqlValue _ (SqlBool (Just True)) = return $ Just (Oid 16, "t", Text)
fromSqlValue _ (SqlBool (Just False)) = return $ Just (Oid 16, "f", Text)
fromSqlValue _ (SqlBool Nothing) = return Nothing
fromSqlValue _ (SqlInt32 Nothing) = return Nothing
fromSqlValue _ (SqlInt32 (Just i)) = return $ Just (Oid 23, B8.pack (show i), Text)
fromSqlValue _ (SqlInt64 Nothing) = return Nothing
fromSqlValue _ (SqlInt64 (Just i)) = return $ Just (Oid 20, B8.pack (show i), Text)
fromSqlValue _ (SqlVarChar Nothing) = return Nothing
fromSqlValue _ (SqlVarChar (Just t)) = return $ Just (Oid 1043, encodeUtf8 t, Binary)
fromSqlValue _ (SqlText Nothing) = return Nothing
fromSqlValue _ (SqlText (Just t)) = return $ Just (Oid 25, encodeUtf8 t, Text)
fromSqlValue _ (SqlUUID Nothing) = return Nothing
fromSqlValue _ (SqlUUID (Just u)) = return $ Just (Oid 2950, U.toASCIIBytes u, Text)
fromSqlValue _ _ = error "fromSqlValue: Parameter conversion failed"

-- | Map field to an SqlValue.
toSqlValue :: (Oid, Maybe ByteString) -> IO SqlValue
toSqlValue (oid, mvalue) =
  case oid of
    Oid 17 -> c P.unescapeBytea SqlByteArray
    Oid 16 -> c (return . readBoolean) SqlBool
    Oid 20 -> c (return . fmap fst . readDecimal) SqlInt64
    Oid 21 -> c (return . fmap fst . readDecimal) SqlInt16
    Oid 23 -> c (return . fmap fst . readDecimal) SqlInt32
    Oid 25 -> c (return . either (const Nothing) Just . decodeUtf8') SqlText
    Oid 700 -> c (return . fmap fst . readDouble) SqlFloating
    Oid 701 -> c (return . fmap fst . readDouble) SqlFloating
    Oid 1042 -> c (return . Just) SqlBlankPaddedString
    Oid 1043 -> c (return . either (const Nothing) Just . decodeUtf8') SqlVarChar
    Oid 2950 -> c (return . U.fromASCIIBytes) SqlUUID

    _ -> return $ Unmatched (oid,mvalue)
  where
    c :: Monad m => (ByteString -> m (Maybe a)) -> (Maybe a -> SqlValue) -> m SqlValue
    c convert construct =
      case mvalue of
        Nothing -> return $ construct Nothing
        Just value -> do
          mvalue' <- convert value
          case mvalue' of
            Nothing -> error "toSqlValue: Conversion failed"
            Just _  -> return $ construct mvalue'

-- | Execute a query with no result.
execSql :: Connection -> ByteString -> [SqlValue] -> IO ()
execSql connection sql parameters =
  ioQuery connection sql parameters (\_ -> return ())

-- | Execute a query an return the number of updated rows (if available).
execSql' :: Connection -> ByteString -> [SqlValue] -> IO (Maybe Int)
execSql' connection sql parameters =
  ioQuery' connection sql parameters (\n _ -> return n)

-- | Error happened during query.
data QueryError = QueryError
    { qeSqlState :: Maybe ByteString
    , qeStatusMessage :: ByteString
    , qeErrorMessage :: Maybe ByteString
    } deriving (Show, Typeable, Generic)

instance Exception QueryError

instance NFData QueryError

-- | Run a query and fold over the results. The action receives an
-- 'InputStream' over all the rows in the result.
ioQuery :: Connection -> ByteString -> [SqlValue] -> (InputStream [SqlValue] -> IO a) -> IO a
ioQuery connection sql parameters f =
    ioQuery' connection sql parameters $ \_ is -> f is

-- | Run a query and fold over the results. The action receives the number of rows affected
-- and an 'InputStream' over all the rows in the result.
ioQuery' :: Connection -> ByteString -> [SqlValue] -> (Maybe Int -> InputStream [SqlValue] -> IO a) -> IO a
ioQuery' connection sql parameters f =
  bracket open done $ \r -> do
    -- Check the status
    status <- P.resultStatus r
    case status of
      CommandOk -> do
        n <- affectedRows r
        go r >>= f n
      TuplesOk  -> do
        n <- affectedRows r
        go r >>= f n
      _ -> do
        -- Extract error information. We need to be careful to
        -- COPY the values here since freeing the result will
        -- cause the "original" values to become garbage.
        sqlState <- fmap (fmap B.copy) $ P.resultErrorField r DiagSqlstate
        statusMessage <- fmap (fmap B.copy) P.resStatus status
        errorMessage <- fmap (fmap B.copy) $ P.resultErrorMessage r
        throw $!! QueryError
                  { qeSqlState = sqlState
                  , qeStatusMessage = statusMessage
                  , qeErrorMessage = errorMessage
                  }

  where
    affectedRows r = do
      !cmdTuples <- P.cmdTuples r
      case cmdTuples of
        Nothing -> return $ Nothing
        Just !x -> return $! fmap fst $! readDecimal $! B.copy x

    done r = P.unsafeFreeResult r

    open = do
      parameters' <- forM parameters $ fromSqlValue connection
      mr <- P.execParams connection sql parameters' Text
      case mr of
        Nothing -> error "No result set; something is very wrong"
        Just r -> return r

    go :: P.Result -> IO (InputStream [SqlValue])
    go r = do
      Col nFields <- liftIO $ P.nfields r
      Row nRows <- liftIO $ P.ntuples r
      let columns = map P.toColumn [0..nFields-1]
      let loop []         = return Nothing
          loop (row:rows) = do
            columnValues <- forM columns $ getSqlVal r row
            return $ Just (columnValues, rows)
      SC.unfoldM loop $ map P.toRow [0..nRows-1]

    getSqlVal r row c = do
      mval <- P.getvalue' r row c
      typ <- P.ftype r c
      toSqlValue (typ,mval)

-- Run a query and result a list of the rows in the result.
runQuery :: Pool Connection -> ByteString -> [SqlValue] -> IO [[SqlValue]]
runQuery connectionPool sql parameters =
  withResource connectionPool $ \c -> do
    ioQuery c sql parameters (\inputStream -> SL.toList inputStream)

-- | Format a message indicating a bad query result due to the "shape".
badQueryResultMsg :: [String] -> [SqlValue] -> String
badQueryResultMsg params columns = concat ["Invalid query result shape. Params: ", show params, ". Result columns: ", show columns]
