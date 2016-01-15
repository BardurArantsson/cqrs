{-# LANGUAGE OverloadedStrings #-}
module Data.CQRS.PostgreSQL.Internal.EventStore
       ( newEventStore
       ) where

import           Control.Exception (throw, catchJust)
import           Control.Monad (forM_)
import           Control.Monad.IO.Class (liftIO)
import           Data.ByteString (ByteString)
import           Data.Pool (Pool)
import           Data.CQRS.Types.EventStore (EventStore(..), StoreError(..))
import           Data.CQRS.Types.PersistedEvent (PersistedEvent(..))
import           Data.CQRS.PostgreSQL.Internal.Utils (execSql, query, SqlValue(..), isDuplicateKey, runTransactionP, badQueryResultMsg)
import           Data.Time.Clock.POSIX (getPOSIXTime)
import           Database.PostgreSQL.LibPQ (Connection)
import           System.IO.Streams (InputStream)
import qualified System.IO.Streams.Combinators as SC

-- Store events for a given aggregate. We do not have a separate table
-- storing aggregate (ID -> version) mappings, which would
-- ordinarily be required to avoid the potential for version "gaps"
-- caused by phantom reads (at any non-SERIALIZABLE isolation level).
-- The scenario would be this: Thread A loads an aggregate and
-- generates some events. Before A commits, but after it has inserted
-- the events into the "event" table, thread B comes along and loads
-- the same aggregate, sees A's events and begins to append events
-- (continuing from A's last sequence number).  When A comes to commit
-- it fails for some reason, but B's commit succeeds (since it is pure
-- inserts there's no data dependency on A to prevent it from
-- committing). Thus we would end up with a gap in the version
-- numbers, not to mention that B may have depended semantically on
-- A's events.  However, in PostgreSQL the initial read that B
-- performs cannot see A's events because READ COMMITTED doesn't
-- permit it, even if the events were inserts.
storeEvents :: Pool Connection -> ByteString -> [PersistedEvent ByteString] -> IO ()
storeEvents cp aggregateId es = do
  translateExceptions aggregateId $ do
    runTransactionP cp $ do
      forM_ es $ \e -> do
        -- Add a timestamp for informational purposes.
        timestamp <- liftIO $ fmap (\t -> round $ t * 1000) $ getPOSIXTime
        -- Insert
        execSql sqlInsertEvent
          [ SqlByteArray $ Just aggregateId
          , SqlByteArray $ Just $ peEvent e
          , SqlInt32 $ Just $ fromIntegral $ peSequenceNumber e
          , SqlInt64 $ Just $ timestamp
          ]

  where
    -- Translate duplicate key exceptions into StoreError.
    translateExceptions aid action =
      catchJust isDuplicateKey action $ \_ ->
        throw $ VersionConflict aid
    -- SQL for event insertion
    sqlInsertEvent =
      "INSERT INTO event (\
      \  aggregate_id, \
      \  event_data, \
      \  seq_no, \
      \  \"timestamp\", \
      \  archive_uuid \
      \) VALUES ($1, $2, $3, $4, NULL)"

retrieveEvents :: Pool Connection -> ByteString -> Int -> (InputStream (PersistedEvent ByteString) -> IO a) -> IO a
retrieveEvents cp aggregateId v0 f =
   runTransactionP cp $ do
     let params = [ SqlByteArray $ Just aggregateId
                  , SqlInt32 $ Just $ fromIntegral v0
                  ]
     query sqlSelectEvent params $ \is ->
       (liftIO $ SC.map unpack is) >>= (liftIO . f)
  where
    unpack [ SqlInt32 (Just sequenceNumber)
           , SqlByteArray (Just eventData)
           ] = PersistedEvent eventData (fromIntegral sequenceNumber)
    unpack columns = error $ badQueryResultMsg [show aggregateId, show v0] columns

    sqlSelectEvent =
        "  SELECT seq_no, event_data \
        \    FROM event \
        \   WHERE aggregate_id = $1 \
        \     AND seq_no > $2 \
        \ORDER BY seq_no ASC"

retrieveAllEvents :: Pool Connection -> (InputStream (ByteString, PersistedEvent ByteString) -> IO a) -> IO a
retrieveAllEvents cp f =
  runTransactionP cp $ do
    query sqlSelectAllEvents [ ] $ \is ->
      (liftIO $ SC.map unpack is) >>= (liftIO . f)
  where
    unpack [ SqlByteArray (Just aggregateId)
           , SqlInt32 (Just sequenceNumber)
           , SqlByteArray (Just eventData)
           ] = (aggregateId, PersistedEvent eventData (fromIntegral sequenceNumber))
    unpack columns = error $ badQueryResultMsg [] columns

    sqlSelectAllEvents =
        "  SELECT aggregate_id, seq_no, event_data \
        \    FROM event \
        \ORDER BY aggregate_id, seq_no ASC"

-- | Create an event store backed by a PostgreSQL connection pool.
-- The database which the connections go to must have an appropriate
-- schema as defined by 'applyMigrations'.
newEventStore :: Pool Connection -> IO (EventStore ByteString ByteString)
newEventStore connectionPool = do
  return $ EventStore
             { esStoreEvents = storeEvents connectionPool
             , esRetrieveEvents = retrieveEvents connectionPool
             , esRetrieveAllEvents = retrieveAllEvents connectionPool
             }
