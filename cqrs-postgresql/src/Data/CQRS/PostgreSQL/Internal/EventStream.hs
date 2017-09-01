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
import           Data.CQRS.PostgreSQL.Internal.Query
import           Data.CQRS.PostgreSQL.Internal.Tables
import           Data.CQRS.PostgreSQL.Internal.Transaction
import           Data.CQRS.PostgreSQL.Metadata
import           Database.PostgreSQL.LibPQ (Connection)
import           NeatInterpolation (text)
import           System.IO.Streams (InputStream)
import qualified System.IO.Streams.Combinators as SC

readEventStream :: Pool Connection -> Tables -> StreamPosition -> (InputStream (StreamPosition, PersistedEvent' ByteString ByteString) -> IO a) -> IO a
readEventStream connectionPool tables sp@(StreamPosition sp0) f =
  runTransactionP connectionPool $
    query sqlReadEvents [ SqlInt64 $ Just sp0 ] $ \is ->
      liftIO (SC.map unpack is) >>= (liftIO . f)
  where
    -- Unpack result columns
    unpack [ SqlInt64 (Just lTimestamp)
           , SqlByteArray (Just aggregateId)
           , SqlByteArray (Just eventData)
           , SqlInt32 (Just sequenceNumber)
           , SqlInt64 (Just timestampMillis)
           ] = (StreamPosition lTimestamp, PersistedEvent' aggregateId (PersistedEvent eventData sequenceNumber timestampMillis))
    unpack columns = error $ badQueryResultMsg [show sp] columns
    -- SQL
    eventTable = tblEvent tables

    sqlReadEvents = [text|
        SELECT "l_timestamp", "aggregate_id", "event_data", "seq_no", "timestamp"
          FROM $eventTable
         WHERE "l_timestamp" > $$1
      ORDER BY "l_timestamp" ASC
    |]

newEventStream :: Pool Connection -> Schema -> IO (EventStream ByteString ByteString)
newEventStream connectionPool schema =
  return EventStream
    { esReadEventStream = readEventStream connectionPool tables
    }
  where
    tables = mkTables schema
