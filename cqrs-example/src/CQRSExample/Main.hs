{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main ( main ) where

import           Control.Concurrent (forkIO, threadDelay)
import           Control.Concurrent.Async (concurrently)
import           Control.Concurrent.STM (atomically, TChan)
import qualified Control.Concurrent.STM.TChan as C
import           Control.Concurrent.STM.TVar (TVar, newTVarIO)
import           Control.Monad (void, forever, when, forM_)
import           Control.Monad.IO.Class (MonadIO(..))
import           Control.Monad.IO.Unlift (MonadUnliftIO)
import           Data.ByteString (ByteString)
import           Data.CQRS.Internal.PersistedEvent (PersistedEvent'(..), shrink)
import qualified Data.CQRS.Memory as M (newEventStream, newStorage, newStorageBackend)
import qualified Data.CQRS.PostgreSQL as P (newEventStream, newStorageBackend, Schema(..))
import           Data.CQRS.PostgreSQL.Migrations (migrate)
import qualified Data.CQRS.Repository as R
import           Data.CQRS.Types.Chunk (Chunk)
import qualified Data.CQRS.Types.Chunk as Chunk
import           Data.CQRS.Types.EventStream (EventStream(..))
import qualified Data.CQRS.Types.EventStream as EventStream
import           Data.CQRS.Types.Iso (Iso)
import qualified Data.CQRS.Types.StorageBackend as SB
import           Data.CQRS.Types.StreamPosition (StreamPosition, infimum)
import           Data.Either (fromRight)
import qualified Data.List.NonEmpty as NEL
import           Data.Maybe (fromMaybe)
import           Data.Pool (createPool, withResource)
import           Data.Serialize (encode, decode)
import qualified Database.PostgreSQL.Harness.Client as H
import qualified Database.PostgreSQL.Simple as PS (connectPostgreSQL, close)
import           Network.Wai.EventSource (ServerEvent(..))
import           System.Environment (getArgs)
import qualified UnliftIO.Streams as Streams
import           UnliftIO.Streams (InputStream)
import           Web.Scotty (scotty)

import           CQRSExample.TaskId (TaskId)
import           CQRSExample.AggregateAction (aggregateAction)
import           CQRSExample.Events
import           CQRSExample.Notifications
import           CQRSExample.Query
import           CQRSExample.Routing

-- Drain an input stream, applying an IO action to each element.
drain :: MonadUnliftIO m => InputStream a -> (a -> m b) -> m (Maybe b)
drain inputStream f = go Nothing
  where
    go b = do
      a <- Streams.read inputStream
      case a of
        Just a' -> fmap Just (f a') >>= go
        Nothing -> return b


-- Source of refresh events to the browser. Never returns.
eventSourcingThread :: TVar QueryState -> EventStream TaskId Event -> TChan ServerEvent -> TChan (Chunk TaskId Event) -> IO ()
eventSourcingThread qs eventStream serverEvents publishedEvents =
  -- Start two threads. One thread just periodically polls the event
  -- stream -- starting at the last position that was successfully
  -- applied to the Query sate. The other thread just processes events
  -- published by the repository. This latter thread never terminates
  -- unless an exception is thrown.
  void $ concurrently (pollEventStream infimum) processPublishedEvents
  where
    pollEventStream :: StreamPosition -> IO ()
    pollEventStream p0 = do
      putStrLn "Polling event stream..."
      -- Process all the events.
      p1 <- esReadEventStream eventStream p0 $ \inputStream ->
        drain inputStream $ \(p, event) -> do
          forM_ (Chunk.fromList (pepAggregateId event) [shrink event]) processEvents
          return p
      -- Next starting position?
      let pn = fromMaybe p0 p1
      -- Go again after a while.
      threadDelay 30000000
      pollEventStream pn

    processPublishedEvents :: IO ()
    processPublishedEvents =
      -- Main event-sourcing loop; never terminates
      forever $ do
        -- Wait for events to be published.
        chunk <- atomically $ C.readTChan publishedEvents
        processEvents chunk
        -- Send out notifications to client
        let (aggregateId, evs) = Chunk.toList chunk
        let notifications = updateNotifications (aggregateId, NEL.toList evs) mempty
        when (notifications /= mempty) $
          atomically $ C.writeTChan serverEvents $! toServerEvent notifications

    processEvents :: Chunk TaskId Event -> IO ()
    processEvents chunk = do
      let (aggregateId, evs) = Chunk.toList chunk
      runQuery qs $ reactToEvents aggregateId $ NEL.toList evs


-- Backend
data Backend = PostgreSQL
             | Memory

-- Start serving the application.
startServing :: Backend -> IO ()
startServing backend = do
  qState <- newTVarIO newQueryState

  -- Queue of json events to send to browser.
  serverEvents <- atomically C.newBroadcastTChan

  -- Events published by query module.
  publishedEvents <- atomically C.newTChan

  -- Publish events
  let publishEvents = atomically . C.writeTChan publishedEvents

  -- Create backend
  (storageBackend, eventStream) <-
    case backend of
      PostgreSQL -> do
        -- Use a pg-harness instance.
        let url = "http://127.0.0.1:8900"
        -- Connection pool creation function. We use a fresh temporary
        -- database for every connection pool.
        connectionString <- H.toConnectionString <$> H.createTemporaryDatabase url
        connectionPool <- createPool (PS.connectPostgreSQL connectionString) PS.close 1 1 5
        -- Use serialization to/from JSON since this event store can only store binary data.
        let identifierIso :: Iso TaskId ByteString = (encode, fromRight (error "Bad encoded identifier data") . decode)
        let eventIso :: Iso Event ByteString = (encode, fromRight (error "Bad encoded event data") . decode)
        -- Create the event stream and event store
        let schema = P.DefaultSchema
        withResource connectionPool $ flip migrate $ schema
        eventStream <- fmap (EventStream.transform identifierIso eventIso) (P.newEventStream connectionPool schema)
        storageBackend <- fmap (SB.transformI identifierIso .  SB.transformE eventIso) $ P.newStorageBackend connectionPool schema
        return (storageBackend, eventStream)
      Memory -> do
        storage <- M.newStorage
        eventStream <- M.newEventStream storage
        storageBackend <- M.newStorageBackend storage
        return (storageBackend, eventStream)

  -- We do not care about snapshots
  let storageBackend' = SB.disableSnapshots storageBackend

  -- Create the resository
  let repositorySettings = R.setSnapshotFrequency 10 R.defaultSettings
  let repository = R.newRepository repositorySettings aggregateAction storageBackend' (liftIO . publishEvents)

  -- Start sourcing events.
  void $ forkIO $
    eventSourcingThread qState eventStream serverEvents publishedEvents

  -- Web serving thread.
  void $ forkIO $
    scotty 8000 $ routes qState repository serverEvents


main :: IO ()
main = do
  -- Run
  let run backend = do
        putStrLn "Starting..."
        startServing backend
        putStrLn "Press <Enter> to quit"
        void $ getLine
  -- Process command line args
  getArgs >>= \case
    ["postgresql"] ->
      run PostgreSQL
    ["memory"] ->
      run Memory
    _ ->
      putStrLn "Usage: cqrs-example [postgresql|memory]"
