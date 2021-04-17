{-# LANGUAGE OverloadedStrings #-}
module Data.CQRS.PostgreSQL.Internal.EventStream
    ( newEventStream
    ) where

import           Control.Monad.IO.Unlift (MonadUnliftIO(..))
import           Control.Monad.Trans.Class (lift)
import           Data.ByteString (ByteString)
import           Data.CQRS.Internal.StreamPosition
import           Data.CQRS.Types.PersistedEvent
import           Data.CQRS.Types.EventStream
import           Data.CQRS.PostgreSQL.Internal.EventStore (retrieveEvents)
import           Data.CQRS.PostgreSQL.Internal.Identifiers
import           Data.CQRS.PostgreSQL.Internal.Query
import           Data.CQRS.PostgreSQL.Internal.Transaction
import           Database.Peregrin.Metadata
import           Database.PostgreSQL.Simple (Connection)
import           UnliftIO.Pool (Pool)
import           UnliftIO.Streams (InputStream)
import qualified UnliftIO.Streams.Combinators as SC

readEventStream :: (MonadUnliftIO m) => Pool Connection -> Identifiers -> StreamPosition -> (InputStream (StreamPosition, PersistedEvent' ByteString ByteString) -> m a) -> m a
readEventStream connectionPool identifiers (StreamPosition sp0) f =
  runTransactionP connectionPool $
    query sqlReadEvents (eventTable, sp0) $ \is ->
      lift $ SC.map unpack is >>= f
  where
    -- Unpack result columns
    unpack (lTimestamp, aggregateId, eventData, sequenceNumber, timestampMillis) =
        (StreamPosition lTimestamp, PersistedEvent' aggregateId (PersistedEvent eventData sequenceNumber timestampMillis))
    -- SQL
    eventTable = tblEvent identifiers

    sqlReadEvents =
      "   SELECT \"l_timestamp\", \"aggregate_id\", \"event_data\", \"seq_no\", \"timestamp\" \
      \     FROM ? \
      \    WHERE \"l_timestamp\" > ? \
      \ ORDER BY \"l_timestamp\" ASC"

newEventStream :: Pool Connection -> Schema -> IO (EventStream ByteString ByteString)
newEventStream connectionPool schema =
  return EventStream
    { esReadEventStream = readEventStream connectionPool identifiers
    , esReadAggregateEvents = retrieveEvents connectionPool identifiers
    }
  where
    identifiers = mkIdentifiers schema
