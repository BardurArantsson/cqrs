{-# LANGUAGE OverloadedStrings #-}
module Data.CQRS.PostgreSQL.Internal.SqlValue
       ( SqlValue(..)
       , fromSqlValue
       , toSqlValue
       ) where

import qualified Data.ByteString.Char8 as B8
import           Data.ByteString (ByteString)
import           Data.ByteString.Lex.Integral (readDecimal, readSigned)
import           Data.Int (Int16, Int32, Int64)
import           Data.Text (Text)
import           Data.Text.Encoding (decodeUtf8', encodeUtf8)
import           Database.PostgreSQL.LibPQ (Connection, Oid(..), Format(..))
import qualified Database.PostgreSQL.LibPQ as LP

-- | Known field types.
data SqlValue = SqlByteArray (Maybe ByteString)
              | SqlBlankPaddedString (Maybe ByteString)
              | SqlBool (Maybe Bool)
              | SqlInt16 (Maybe Int16)
              | SqlInt32 (Maybe Int32)
              | SqlInt64 (Maybe Int64)
              | SqlVarChar (Maybe Text)
              | SqlText (Maybe Text)
              | Unmatched (Oid, Maybe ByteString)
              deriving (Eq, Show)

-- | Read a boolean.
readBoolean :: ByteString -> Maybe Bool
readBoolean "t" = Just True
readBoolean "f" = Just False
readBoolean _ = Nothing

-- | Map an SqlValue to a parameter.
fromSqlValue :: Connection -> SqlValue -> IO (Maybe (Oid, ByteString, Format))
fromSqlValue connection (SqlByteArray a) =
  case a of
    Nothing -> return Nothing
    Just a' -> do
      x <- LP.escapeByteaConn connection a'
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
fromSqlValue _ _ = error "fromSqlValue: Parameter conversion failed"

-- | Map field to an SqlValue.
toSqlValue :: (Oid, Maybe ByteString) -> IO SqlValue
toSqlValue (oid, mvalue) =
  case oid of
    Oid 17 -> c LP.unescapeBytea SqlByteArray
    Oid 16 -> c (return . readBoolean) SqlBool
    Oid 20 -> c (return . fmap fst . readSigned readDecimal) SqlInt64
    Oid 21 -> c (return . fmap fst . readSigned readDecimal) SqlInt16
    Oid 23 -> c (return . fmap fst . readSigned readDecimal) SqlInt32
    Oid 25 -> c (return . either (const Nothing) Just . decodeUtf8') SqlText
    Oid 1042 -> c (return . Just) SqlBlankPaddedString
    Oid 1043 -> c (return . either (const Nothing) Just . decodeUtf8') SqlVarChar

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
