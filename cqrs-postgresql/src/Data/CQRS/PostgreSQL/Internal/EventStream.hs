{-# LANGUAGE OverloadedStrings #-}
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
import           Database.PostgreSQL.LibPQ (Connection)
import           System.IO.Streams (InputStream)
import qualified System.IO.Streams.Combinators as SC

readEventStream :: Pool Connection -> Maybe StreamPosition -> (InputStream (StreamPosition, ByteString, PersistedEvent ByteString) -> IO a) -> IO a
readEventStream connectionPool maybeStartingPosition f = do
  -- Figure out the starting position
  let i0 = case maybeStartingPosition of
             Just (StreamPosition i) -> i
             Nothing                 -> 0 -- BIGSERIAL starts at 1
  -- Run the query
  runTransactionP connectionPool $ do
    query sqlReadEvents [ SqlInt64 $ Just i0 ] $ \is -> do
      (liftIO $ SC.map unpack is) >>= (liftIO . f)
  where
    -- Unpack result columns
    unpack [ SqlInt64 (Just lTimestamp)
           , SqlByteArray (Just aggregateId)
           , SqlByteArray (Just eventData)
           , SqlInt32 (Just sequenceNumber)
           ] = (StreamPosition lTimestamp, aggregateId, PersistedEvent eventData (fromIntegral sequenceNumber))
    unpack columns = error $ badQueryResultMsg [show maybeStartingPosition] columns
    -- SQL
    sqlReadEvents =
        "  SELECT l_timestamp, aggregate_id, event_data, seq_no \
        \    FROM event \
        \   WHERE l_timestamp > $1 \
        \ORDER BY l_timestamp ASC"

newEventStream :: Pool Connection -> IO (EventStream ByteString ByteString)
newEventStream connectionPool = do
  return $ EventStream
    { esReadEventStream = readEventStream connectionPool
    }
