{-# LANGUAGE OverloadedStrings #-}
module Data.CQRS.PostgreSQL.Internal.EventStore
       ( newEventStore
       ) where

import           Control.Monad (forM_)
import           Control.Monad.IO.Unlift (MonadUnliftIO(..), liftIO)
import           Data.ByteString (ByteString)
import           Data.CQRS.Internal.PersistedEvent
import           Data.CQRS.Types.Chunk (Chunk)
import qualified Data.CQRS.Types.Chunk as C
import           Data.CQRS.Types.EventStore (EventStore(..), StoreError(..))
import           Data.CQRS.PostgreSQL.Internal.Identifiers
import           Data.CQRS.PostgreSQL.Internal.Query
import           Data.CQRS.PostgreSQL.Internal.Transaction
import           Data.Int (Int32)
import           Database.Peregrin.Metadata (Schema)
import           Database.PostgreSQL.Simple (Connection, SqlError(..), Only(..), Binary(..))
import           System.IO.Streams (InputStream)
import qualified System.IO.Streams.Combinators as SC
import           UnliftIO.Exception (throwIO, catchJust)
import           UnliftIO.Pool (Pool)

-- | Is the given 'SqlError' a duplicate key exception? This
-- function is meant for use with 'catchJust'.
isDuplicateKey :: SqlError -> Maybe ()
isDuplicateKey se | sqlState se == "23505" = Just ()
                  | otherwise              = Nothing

-- Store events for a given aggregate. We do not have a separate table
-- storing aggregate (ID -> version) mappings, which would ordinarily
-- be required to avoid the potential for version "gaps" caused by
-- phantom reads (at any non-SERIALIZABLE isolation level).  The
-- scenario would be this: Thread A loads an aggregate and generates
-- some events. Before A commits, but after it has inserted the events
-- into the "event" table, thread B comes along and loads the same
-- aggregate, sees A's events and begins to append events (continuing
-- from A's last sequence number).  When A comes to commit it fails
-- for some reason, but B's commit succeeds (since it is pure inserts
-- there's no data dependency on A to prevent it from
-- committing). Thus we would end up with a gap in the version
-- numbers, not to mention that B may have depended semantically on
-- A's events.  However, in PostgreSQL the initial read that B
-- performs cannot see A's events because PostgreSQL's version of
-- REPEATABLE READ prevents phantom reads.
storeEvents :: (MonadUnliftIO m) => Pool Connection -> Identifiers -> Chunk ByteString ByteString -> m ()
storeEvents cp identifiers chunk =
  translateExceptions aggregateId $
    runTransactionP cp $
      forM_ events $ \e ->
        -- Insert. We ignore the aggregateID specified on the actual
        -- events because it must (by contract) be exactly the same as
        -- the 'aggregateId' parameter.
        execute sqlInsertEvent
          ( eventTable
          , Binary aggregateId
          , Binary $ peEvent e
          , peSequenceNumber e
          , peTimestampMillis e
          )

  where
    (aggregateId, events) = C.toList chunk
    -- Translate duplicate key exceptions into StoreError.
    translateExceptions aid action =
      catchJust isDuplicateKey action $ \_ ->
        throwIO $ VersionConflict aid
    -- SQL for event insertion
    eventTable = tblEvent identifiers
    sqlInsertEvent =
      "INSERT INTO ? \
      \            (\"aggregate_id\", \"event_data\", \"seq_no\", \"timestamp\") \
      \     VALUES (?, ?, ?, ?)"

retrieveEvents :: (MonadUnliftIO m) => Pool Connection -> Identifiers -> ByteString -> Int32 -> (InputStream (PersistedEvent ByteString) -> m a) -> m a
retrieveEvents cp identifiers aggregateId v0 f =
  withRunInIO $ \io ->
    runTransactionP cp $
      query sqlSelectEvent (eventTable, Binary aggregateId, v0) $ \is ->
        liftIO $ SC.map unpack is >>= (io . f)
  where
    unpack (eventData, sequenceNumber, timestampMillis) =
      PersistedEvent eventData sequenceNumber timestampMillis

    eventTable = tblEvent identifiers

    sqlSelectEvent =
      "  SELECT \"event_data\", \"seq_no\", \"timestamp\" \
      \    FROM ? \
      \   WHERE \"aggregate_id\" = ? \
      \     AND \"seq_no\" > ? \
      \ORDER BY \"seq_no\" ASC"

retrieveAllEvents :: (MonadUnliftIO m) => Pool Connection -> Identifiers -> (InputStream (PersistedEvent' ByteString ByteString) -> m a) -> m a
retrieveAllEvents cp identifiers f =
  withRunInIO $ \io ->
    runTransactionP cp $
      query sqlSelectAllEvents (Only eventTable) $ \is ->
        liftIO $ SC.map unpack is >>= (io . f)
  where
    unpack (aggregateId, sequenceNumber, eventData, timestampMillis) =
        grow aggregateId $ PersistedEvent eventData sequenceNumber timestampMillis

    eventTable = tblEvent identifiers

    sqlSelectAllEvents =
      "  SELECT \"aggregate_id\", \"seq_no\", \"event_data\", \"timestamp\" \
      \    FROM ? \
      \ORDER BY \"aggregate_id\", \"seq_no\" ASC"

-- | Create an event store backed by a PostgreSQL connection pool.
-- The database which the connections go to must have an appropriate
-- schema as defined by 'applyMigrations'.
newEventStore :: Pool Connection -> Schema -> IO (EventStore ByteString ByteString)
newEventStore connectionPool schema =
  return EventStore
    { esStoreEvents = storeEvents connectionPool identifiers
    , esRetrieveEvents = retrieveEvents connectionPool identifiers
    , esRetrieveAllEvents = retrieveAllEvents connectionPool identifiers
    }
  where
    identifiers = mkIdentifiers schema
