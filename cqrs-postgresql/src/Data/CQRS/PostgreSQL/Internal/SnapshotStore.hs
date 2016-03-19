{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Data.CQRS.PostgreSQL.Internal.SnapshotStore
    ( newSnapshotStore
    ) where

import           Control.Exception (catchJust)
import           Control.Monad.IO.Class (liftIO)
import           Data.ByteString (ByteString)
import           Data.CQRS.Types.Snapshot (Snapshot(..))
import           Data.CQRS.Types.SnapshotStore (SnapshotStore(..))
import           Data.CQRS.PostgreSQL.Internal.Utils
import           Data.CQRS.PostgreSQL.Internal.Tables
import           Data.CQRS.PostgreSQL.Metadata
import           Data.Pool (Pool)
import           Database.PostgreSQL.LibPQ (Connection)
import           NeatInterpolation (text)
import qualified System.IO.Streams as Streams

writeSnapshot :: Pool Connection -> Tables -> ByteString -> Snapshot ByteString -> IO ()
writeSnapshot connectionPool tables aggregateId (Snapshot v d) =
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
    snapshotTable = tblSnapshot tables

    updateSnapshotSql = [text|
      UPDATE $snapshotTable
         SET "data" = $$1
           , "version" = $$2
       WHERE "aggregate_id" = $$3
    |]

    insertSnapshotSql = [text|
      INSERT INTO $snapshotTable
                  ("aggregate_id", "data", "version")
           VALUES ($$1, $$2, $$3)
    |]

    -- Run an action, ignoring duplicate key exceptions.
    ignoreDuplicateKey action =
      catchJust isDuplicateKey action $ \_ ->
        return () -- Ignore

readSnapshot :: Pool Connection -> Tables -> ByteString -> IO (Maybe (Snapshot ByteString))
readSnapshot connectionPool tables aggregateId = do
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
    snapshotTable = tblSnapshot tables

    selectSnapshotSql = [text|
      SELECT "data", "version"
        FROM $snapshotTable
       WHERE "aggregate_id" = $$1
    |]

-- | Create an snapshot store backed by a PostgreSQL connection pool.
newSnapshotStore :: Pool Connection -> Schema -> IO (SnapshotStore ByteString ByteString)
newSnapshotStore connectionPool schema = do
  return $ SnapshotStore
    (writeSnapshot connectionPool tables)
    (readSnapshot connectionPool tables)
  where
    tables = mkTables schema

