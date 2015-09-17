{-# LANGUAGE OverloadedStrings #-}
module Data.CQRS.PostgreSQL.Internal.SnapshotStore
    ( newSnapshotStore
    ) where

import           Control.Exception (catchJust)
import           Data.ByteString (ByteString)
import           Data.Pool (Pool, withResource)
import           Data.CQRS.Types.Snapshot (Snapshot(..))
import           Data.CQRS.Types.SnapshotStore (SnapshotStore(..))
import           Data.CQRS.PostgreSQL.Internal.Utils (execSql, execSql', ioQuery, isDuplicateKey, withTransaction, badQueryResultMsg, SqlValue(..))
import           Data.UUID.Types (UUID)
import           Database.PostgreSQL.LibPQ (Connection)
import qualified System.IO.Streams as Streams

writeSnapshot :: Pool Connection -> UUID -> Snapshot ByteString -> IO ()
writeSnapshot connectionPool aggregateId (Snapshot v d) =
  withResource connectionPool $ \c -> do
    -- We ignore duplicate key exceptions since snapshots aren't
    -- important enough to merit aborting the user's command. We'll
    -- probably have opportunities to write a snapshot for the
    -- aggregate later anyway.
    ignoreDuplicateKey $
      withTransaction c $ do
        maybeUpdatedRows <- execSql' c updateSnapshotSql
          [ SqlByteArray (Just d)
          , SqlInt32 $ Just $ fromIntegral v
          , SqlUUID $ Just aggregateId
          ]
        if (maybe 0 id maybeUpdatedRows) > 0 then
          return () -- Update happened, done!
          else
            -- Not updated; this means that the snapshot row didn't exist,
            -- so we'll try to INSERT it now.
            execSql c insertSnapshotSql
              [ SqlUUID $ Just aggregateId
              , SqlByteArray (Just d)
              , SqlInt32 $ Just $ fromIntegral v
              ]

  where
    updateSnapshotSql =
      "UPDATE snapshot SET data=$1, version=$2 WHERE aggregate_uuid=$3"
    insertSnapshotSql =
      "INSERT INTO snapshot \
      \   (aggregate_uuid, data, version) \
      \   VALUES ($1, $2, $3)"
    -- Run an action, ignoring duplicate key exceptions.
    ignoreDuplicateKey action =
      catchJust isDuplicateKey action $ \_ ->
        return () -- Ignore

readSnapshot :: Pool Connection -> UUID -> IO (Maybe (Snapshot ByteString))
readSnapshot connectionPool aggregateId = do
  withResource connectionPool $ \connection -> do
    withTransaction connection $ do
      -- Unpack columns from result.
      let unpackColumns :: [SqlValue] -> (ByteString, Int)
          unpackColumns [ SqlByteArray (Just d)
                        , SqlInt32 (Just v) ] = (d, fromIntegral v)
          unpackColumns columns               = error $ badQueryResultMsg [show aggregateId] columns
      -- Run the query.
      r <- ioQuery connection selectSnapshotSql [SqlUUID $ Just aggregateId] $ \inputStream -> do
             cs <- Streams.read inputStream
             return $ fmap unpackColumns cs
      case r of
        Just (d,v) -> return $ Just $ Snapshot v d
        Nothing    -> return Nothing

  where
    selectSnapshotSql =
      "SELECT data, version FROM snapshot WHERE aggregate_uuid=$1;"

-- | Create an snapshot store backed by a PostgreSQL connection pool.
newSnapshotStore :: Pool Connection -> IO (SnapshotStore ByteString)
newSnapshotStore connectionPool = do
  return $ SnapshotStore
    (writeSnapshot connectionPool)
    (readSnapshot connectionPool)
