{-# LANGUAGE OverloadedStrings #-}
module Data.CQRS.PostgreSQL.Internal.SnapshotStore
    ( newSnapshotStore
    ) where

import           Control.Exception (catchJust)
import           Control.Monad.IO.Class (liftIO)
import           Data.ByteString (ByteString)
import           Data.CQRS.Types.Snapshot (Snapshot(..))
import           Data.CQRS.Types.SnapshotStore (SnapshotStore(..))
import           Data.CQRS.PostgreSQL.Internal.Utils (execSql, execSql', query, isDuplicateKey, runTransactionP, badQueryResultMsg, SqlValue(..))
import           Data.Pool (Pool)
import           Database.PostgreSQL.LibPQ (Connection)
import qualified System.IO.Streams as Streams

writeSnapshot :: Pool Connection -> ByteString -> Snapshot ByteString -> IO ()
writeSnapshot connectionPool aggregateId (Snapshot v d) =
  -- We ignore duplicate key exceptions since snapshots aren't
  -- important enough to merit aborting the user's command. We'll
  -- probably have opportunities to write a snapshot for the
  -- aggregate later anyway.
  ignoreDuplicateKey $
    runTransactionP connectionPool $ do
      maybeUpdatedRows <- execSql' updateSnapshotSql
        [ SqlByteArray (Just d)
        , SqlInt32 $ Just $ fromIntegral v
        , SqlByteArray $ Just aggregateId
        ]
      if (maybe 0 id maybeUpdatedRows) > 0 then
        return () -- Update happened, done!
        else
          -- Not updated; this means that the snapshot row didn't exist,
          -- so we'll try to INSERT it now.
          execSql insertSnapshotSql
            [ SqlByteArray $ Just aggregateId
            , SqlByteArray (Just d)
            , SqlInt32 $ Just $ fromIntegral v
            ]

  where
    updateSnapshotSql =
      "UPDATE snapshot SET data=$1, version=$2 WHERE aggregate_id=$3"
    insertSnapshotSql =
      "INSERT INTO snapshot \
      \   (aggregate_id, data, version) \
      \   VALUES ($1, $2, $3)"
    -- Run an action, ignoring duplicate key exceptions.
    ignoreDuplicateKey action =
      catchJust isDuplicateKey action $ \_ ->
        return () -- Ignore

readSnapshot :: Pool Connection -> ByteString -> IO (Maybe (Snapshot ByteString))
readSnapshot connectionPool aggregateId = do
  runTransactionP connectionPool $ do
    -- Unpack columns from result.
    let unpackColumns :: [SqlValue] -> (ByteString, Int)
        unpackColumns [ SqlByteArray (Just d)
                      , SqlInt32 (Just v) ] = (d, fromIntegral v)
        unpackColumns columns               = error $ badQueryResultMsg [show aggregateId] columns
    -- Run the query.
    r <- query selectSnapshotSql [SqlByteArray $ Just aggregateId] $ \inputStream -> do
           cs <- liftIO $ Streams.read inputStream
           return $ fmap unpackColumns cs
    case r of
      Just (d,v) -> return $ Just $ Snapshot v d
      Nothing    -> return Nothing

  where
    selectSnapshotSql =
      "SELECT data, version FROM snapshot WHERE aggregate_id=$1;"

-- | Create an snapshot store backed by a PostgreSQL connection pool.
newSnapshotStore :: Pool Connection -> IO (SnapshotStore ByteString ByteString)
newSnapshotStore connectionPool = do
  return $ SnapshotStore
    (writeSnapshot connectionPool)
    (readSnapshot connectionPool)
