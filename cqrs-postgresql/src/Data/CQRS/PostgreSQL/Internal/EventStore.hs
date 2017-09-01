{-# LANGUAGE QuasiQuotes #-}
module Data.CQRS.PostgreSQL.Internal.EventStore
       ( newEventStore
       ) where

import           Control.Exception (throw, catchJust)
import           Control.Monad (forM_)
import           Control.Monad.IO.Class (liftIO)
import           Data.ByteString (ByteString)
import           Data.Pool (Pool)
import           Data.CQRS.Internal.PersistedEvent
import           Data.CQRS.Types.Chunk (Chunk)
import qualified Data.CQRS.Types.Chunk as C
import           Data.CQRS.Types.EventStore (EventStore(..), StoreError(..))
import           Data.CQRS.PostgreSQL.Internal.QueryError
import           Data.CQRS.PostgreSQL.Internal.Query
import           Data.CQRS.PostgreSQL.Internal.Tables
import           Data.CQRS.PostgreSQL.Internal.Transaction
import           Data.CQRS.PostgreSQL.Metadata
import           Data.Int (Int32)
import           Database.PostgreSQL.LibPQ (Connection)
import           System.IO.Streams (InputStream)
import qualified System.IO.Streams.Combinators as SC
import           NeatInterpolation (text)

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
storeEvents :: Pool Connection -> Tables -> Chunk ByteString ByteString -> IO ()
storeEvents cp tables chunk =
  translateExceptions aggregateId $
    runTransactionP cp $
      forM_ events $ \e ->
        -- Insert. We ignore the aggregateID specified on the actual
        -- events because it must (by contract) be exactly the same as
        -- the 'aggregateId' parameter.
        execute sqlInsertEvent
          [ SqlByteArray $ Just aggregateId
          , SqlByteArray $ Just $ peEvent e
          , SqlInt32 $ Just $ peSequenceNumber e
          , SqlInt64 $ Just $ peTimestampMillis e
          ]

  where
    (aggregateId, events) = C.toList chunk
    -- Translate duplicate key exceptions into StoreError.
    translateExceptions aid action =
      catchJust isDuplicateKey action $ \_ ->
        throw $ VersionConflict aid
    -- SQL for event insertion
    eventTable = tblEvent tables
    sqlInsertEvent = [text|
      INSERT INTO $eventTable
                  ("aggregate_id", "event_data", "seq_no", "timestamp")
           VALUES ($$1, $$2, $$3, $$4)
    |]

retrieveEvents :: Pool Connection -> Tables -> ByteString -> Int32 -> (InputStream (PersistedEvent ByteString ByteString) -> IO a) -> IO a
retrieveEvents cp tables aggregateId v0 f =
   runTransactionP cp $ do
     let params = [ SqlByteArray $ Just aggregateId
                  , SqlInt32 $ Just v0
                  ]
     query sqlSelectEvent params $ \is ->
       liftIO (SC.map unpack is) >>= (liftIO . f)
  where
    unpack [ SqlInt32 (Just sequenceNumber)
           , SqlByteArray (Just eventData)
           , SqlInt64 (Just timestampMillis)
           ] = PersistedEvent eventData sequenceNumber timestampMillis
    unpack columns = error $ badQueryResultMsg [show aggregateId, show v0] columns

    eventTable = tblEvent tables

    sqlSelectEvent = [text|
        SELECT "seq_no", "event_data", "timestamp"
          FROM $eventTable
         WHERE "aggregate_id" = $$1
           AND "seq_no" > $$2
      ORDER BY "seq_no" ASC
    |]

retrieveAllEvents :: Pool Connection -> Tables -> (InputStream (PersistedEvent' ByteString ByteString) -> IO a) -> IO a
retrieveAllEvents cp tables f =
  runTransactionP cp $
    query sqlSelectAllEvents [ ] $ \is ->
      liftIO (SC.map unpack is) >>= (liftIO . f)
  where
    unpack [ SqlByteArray (Just aggregateId)
           , SqlInt32 (Just sequenceNumber)
           , SqlByteArray (Just eventData)
           , SqlInt64 (Just timestampMillis)
           ] = grow aggregateId $ PersistedEvent eventData sequenceNumber timestampMillis
    unpack columns = error $ badQueryResultMsg [] columns

    eventTable = tblEvent tables

    sqlSelectAllEvents = [text|
        SELECT "aggregate_id", "seq_no", "event_data", "timestamp"
          FROM $eventTable
      ORDER BY "aggregate_id", "seq_no" ASC
    |]

-- | Create an event store backed by a PostgreSQL connection pool.
-- The database which the connections go to must have an appropriate
-- schema as defined by 'applyMigrations'.
newEventStore :: Pool Connection -> Schema -> IO (EventStore ByteString ByteString)
newEventStore connectionPool schema =
  return EventStore
    { esStoreEvents = storeEvents connectionPool tables
    , esRetrieveEvents = retrieveEvents connectionPool tables
    , esRetrieveAllEvents = retrieveAllEvents connectionPool tables
    }
  where
    tables = mkTables schema
