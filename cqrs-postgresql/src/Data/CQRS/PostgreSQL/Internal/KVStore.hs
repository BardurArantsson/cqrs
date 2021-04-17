{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.CQRS.PostgreSQL.Internal.KVStore
    ( newKVStore
    ) where

import           Control.Monad.IO.Unlift (MonadUnliftIO(..))
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Reader (ReaderT(..), ask)
import           Data.Aeson (FromJSON, ToJSON, Result(..), toJSON, fromJSON)
import           Data.ByteString (ByteString)
import           Data.CQRS.KVStore
import           Data.CQRS.PostgreSQL.Internal.Identifiers
import           Data.CQRS.PostgreSQL.Internal.Query
import           Data.CQRS.PostgreSQL.Internal.Transaction
import           Data.Traversable (forM)
import           Database.Peregrin.Metadata (Schema, QIdentifier)
import           Database.PostgreSQL.Simple (Connection, Only(..), Binary(..))
import           Prelude hiding (lookup, traverse)
import           UnliftIO.Exception (throwIO)
import           UnliftIO.Pool (Pool)
import           UnliftIO.Streams (InputStream)
import qualified UnliftIO.Streams.Combinators as SC

unpackResult :: (MonadUnliftIO m) => ByteString -> Result a -> m a
unpackResult _ (Success a) = return a
unpackResult k (Error msg) = throwIO $ UnreadableValue k msg

type KVStoreT m a = ReaderT QIdentifier (QueryT m) a

askKVTable :: Monad m => KVStoreT m QIdentifier
askKVTable = ask

adjust :: (MonadUnliftIO m, FromJSON a, ToJSON a) => (a -> a) -> ByteString -> KVStoreT m ()
adjust f = adjustWithKey (\_ a -> f a)

adjustWithKey :: (MonadUnliftIO m, FromJSON a, ToJSON a) => (ByteString -> a -> a) -> ByteString -> KVStoreT m ()
adjustWithKey f k =
  alter (fmap $ f k) k

alter :: (MonadUnliftIO m, FromJSON a, ToJSON a) => (Maybe a -> Maybe a) -> ByteString -> KVStoreT m ()
alter f k = do
  maybeA <- lookup k
  case (maybeA, f maybeA) of
    (Nothing, Nothing) ->
      pure () -- Nothing to do
    (Just _, Nothing) ->
      delete k
    (_, Just a') ->
      insert k a' -- Insert is really UPSERT, so it works for both cases here

delete :: MonadUnliftIO m => ByteString -> KVStoreT m ()
delete k = do
  kvTable <- askKVTable
  lift $ execute sqlDelete
    ( kvTable
    , Binary k
    )

  where
    sqlDelete =
      "DELETE FROM ? \
      \      WHERE \"key\" = ?"

insert :: (MonadUnliftIO m, ToJSON a) => ByteString -> a -> KVStoreT m ()
insert k a = do
  kvTable <- askKVTable
  lift $ execute sqlUpsert
    ( kvTable
    , Binary k
    , toJSON a
    )

  where
    sqlUpsert =
      "INSERT INTO ? \
      \            (\"key\", \"value\") \
      \     VALUES (?, ?) \
      \ON CONFLICT (key)  DO UPDATE \
      \        SET \"value\" = EXCLUDED.\"value\" "

insertWith :: (MonadUnliftIO m, FromJSON a, ToJSON a) => (a -> a -> a) -> ByteString -> a -> KVStoreT m ()
insertWith f k a =
  alter (\case Just a0 -> Just $ f a a0
               Nothing -> Just a) k

lookup :: (MonadUnliftIO m, FromJSON a) => ByteString -> KVStoreT m (Maybe a)
lookup k = do
  kvTable <- askKVTable
  result <- fmap (fmap $ fromJSON . fromOnly) $ lift $ query1 sqlSelectValue (kvTable, Binary k)
  forM result (unpackResult k)

  where
    sqlSelectValue =
      "  SELECT \"value\" \
      \    FROM ? \
      \   WHERE \"key\" = ?"

traverse :: (MonadUnliftIO m, FromJSON a) => (InputStream (ByteString, a) -> m b) -> KVStoreT m b
traverse f = do
  kvTable <- askKVTable
  lift $ query sqlSelectKeyValues (Only kvTable) $ \is -> do
    SC.mapM unpack is >>= (lift . f)

  where
    unpack (Binary key, json) = lift $ do
      v <- unpackResult key (fromJSON json)
      return (key, v)

    sqlSelectKeyValues =
      "  SELECT \"key\", \"value\" \
      \    FROM ? "


newKVStore :: (MonadUnliftIO m, FromJSON a, ToJSON a) => Pool Connection -> Schema -> m (KVStore ByteString a)
newKVStore pool schema =
  return KVStore
    { kvsAdjust = \f k -> transact $ adjust f k
    , kvsAdjustWithKey = \f k -> transact $ adjustWithKey f k
    , kvsAlter = \f k -> transact $ alter f k
    , kvsDelete = transact . delete
    , kvsInsert = \k a -> transact $ insert k a
    , kvsInsertWith = \f k a -> transact $ insertWith f k a
    , kvsLookup = transact . lookup
    , kvsTraverse = transact . traverse
    }

  where

    identifiers = mkIdentifiers schema

    transact f =
      runTransactionP pool $
        runReaderT f $ tblKvStore identifiers
