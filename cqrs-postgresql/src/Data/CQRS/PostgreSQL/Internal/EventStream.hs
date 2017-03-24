{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Data.CQRS.PostgreSQL.Internal.EventStream
    ( newEventStream
    ) where

import           Control.Monad.IO.Class (liftIO)
import           Data.ByteString (ByteString)
import           Data.Pool (Pool)
import           Data.CQRS.Internal.StreamPosition
import           Data.CQRS.Types.PersistedEvent
import           Data.CQRS.Types.EventStream
import           Data.CQRS.PostgreSQL.Internal.Utils
import           Data.CQRS.PostgreSQL.Internal.Tables
import           Data.CQRS.PostgreSQL.Metadata
import           Database.PostgreSQL.LibPQ (Connection)
import           NeatInterpolation (text)
import           System.IO.Streams (InputStream)
import qualified System.IO.Streams.Combinators as SC

readEventStream :: Pool Connection -> Tables -> StreamPosition -> (InputStream (StreamPosition, PersistedEvent ByteString ByteString) -> IO a) -> IO a
readEventStream connectionPool tables sp@(StreamPosition sp0) f = do
  -- Run the query
  runTransactionP connectionPool $ do
    query sqlReadEvents [ SqlInt64 $ Just sp0 ] $ \is -> do
      (liftIO $ SC.map unpack is) >>= (liftIO . f)
  where
    -- Unpack result columns
    unpack [ SqlInt64 (Just lTimestamp)
           , SqlByteArray (Just aggregateId)
           , SqlByteArray (Just eventData)
           , SqlInt32 (Just sequenceNumber)
           ] = (StreamPosition lTimestamp, PersistedEvent eventData sequenceNumber aggregateId)
    unpack columns = error $ badQueryResultMsg [show sp] columns
    -- SQL
    eventTable = tblEvent tables

    sqlReadEvents = [text|
        SELECT "l_timestamp", "aggregate_id", "event_data", "seq_no"
          FROM $eventTable
         WHERE "l_timestamp" > $$1
      ORDER BY "l_timestamp" ASC
    |]

newEventStream :: Pool Connection -> Schema -> IO (EventStream ByteString ByteString)
newEventStream connectionPool schema = do
  return $ EventStream
    { esReadEventStream = readEventStream connectionPool tables
    }
  where
    tables = mkTables schema
