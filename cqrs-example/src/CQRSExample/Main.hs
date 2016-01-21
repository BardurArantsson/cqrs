module Main ( main ) where

import           Control.Concurrent (forkIO, threadDelay)
import           Control.Concurrent.Async (concurrently)
import           Control.Concurrent.STM (atomically, TChan)
import qualified Control.Concurrent.STM.TChan as C
import           Control.Concurrent.STM.TVar (TVar, newTVarIO)
import           Control.Monad (void, forever, when)
import           Data.CQRS.Memory (newEventStore, newEventStream, newStorage)
import           Data.CQRS.SnapshotStore (nullSnapshotStore)
import qualified Data.CQRS.Repository as R
import           Data.CQRS.Types.PersistedEvent
import           Data.CQRS.Types.EventStream (EventStream(..), StreamPosition)
import           Data.Maybe (isJust)
import           Network.Wai.EventSource (ServerEvent(..))
import qualified System.IO.Streams as Streams
import           System.IO.Streams (InputStream)
import           Web.Scotty (scotty)

import           CQRSExample.TaskId (TaskId)
import           CQRSExample.AggregateAction (aggregateAction)
import           CQRSExample.Events
import           CQRSExample.Notifications
import           CQRSExample.Query
import           CQRSExample.Routing

-- Drain an input stream, applying an IO action to each element.
drain :: InputStream a -> (a -> IO b) -> IO (Maybe b)
drain inputStream f = go Nothing
  where
    go b = do
      a <- Streams.read inputStream
      case a of
        Just a' -> fmap Just (f a') >>= go
        Nothing -> return b


-- Source of refresh events to the browser. Never returns.
eventSourcingThread :: TVar QueryState -> EventStream TaskId Event -> TChan ServerEvent -> TChan (TaskId, [PersistedEvent Event]) -> IO ()
eventSourcingThread qs eventStream serverEvents publishedEvents = do
  -- Start two threads. One thread just periodically polls the event
  -- stream -- starting at the last position that was successfully
  -- applied to the Query sate. The other thread just processes events
  -- published by the repository. This latter thread never terminates
  -- unless an exception is thrown.
  void $ concurrently (pollEventStream Nothing) processPublishedEvents
  where
    pollEventStream :: Maybe StreamPosition -> IO ()
    pollEventStream p0 = do
      putStrLn "Polling event stream..."
      -- Process all the events.
      p1 <- (esReadEventStream eventStream) p0 $ \inputStream -> do
        drain inputStream $ \(p, aggregateId, event) -> do
          processEvents aggregateId [event]
          return p
      -- Next starting position?
      let pn = if isJust p1 then p1 else p0
      -- Go again after a while.
      threadDelay 30000000
      pollEventStream pn

    processPublishedEvents :: IO ()
    processPublishedEvents = do
      -- Main event-sourcing loop; never terminates
      forever $ do
        -- Wait for events to be published.
        (aggregateId, evs) <- atomically $ C.readTChan publishedEvents
        processEvents aggregateId evs
        -- Send out notifications to client
        let notifications = updateNotifications (aggregateId, evs) mempty
        when (notifications /= mempty) $ do
          atomically $ C.writeTChan serverEvents $! toServerEvent $ notifications

    processEvents :: TaskId -> [PersistedEvent Event] -> IO ()
    processEvents aggregateId evs = do
      -- Supply to Query to update its state.
      runQuery qs $ reactToEvents aggregateId evs

-- Start serving the application.
startServing :: IO ()
startServing = do
  qState <- newTVarIO newQueryState

  -- Queue of json events to send to browser.
  serverEvents <- atomically $ C.newBroadcastTChan

  -- Events published by query module.
  publishedEvents <- atomically $ C.newTChan

  -- Publish events
  let publishEvents events = atomically $ C.writeTChan publishedEvents events

  -- Create memory CQRS backing storage
  storage <- newStorage
  eventStore <- newEventStore storage
  eventStream <- newEventStream storage

  -- Create the resository
  let repositorySettings = R.setSnapshotFrequency 10 $ R.defaultSettings
  let repository = R.newRepository repositorySettings aggregateAction eventStore nullSnapshotStore publishEvents

  -- Start sourcing events.
  void $ forkIO $ do
    eventSourcingThread qState eventStream serverEvents publishedEvents

  -- Web serving thread.
  void $ forkIO $ do
    scotty 8000 $ routes qState repository serverEvents


main :: IO ()
main = do
  putStrLn "Starting..."
  startServing
  putStrLn "Press <Enter> to quit"
  _ <- getLine
  return ()
