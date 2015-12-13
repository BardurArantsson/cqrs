module Main ( main ) where

import           Control.Concurrent (forkIO, threadDelay)
import           Control.Concurrent.Async (concurrently)
import           Control.Concurrent.STM (atomically, TChan)
import qualified Control.Concurrent.STM.TChan as C
import           Control.Concurrent.STM.TVar (TVar, newTVarIO)
import           Control.Monad (void, forever, when)
import           Data.CQRS.Memory (newEventStore, newArchiveStore, newStorage)
import           Data.CQRS.SnapshotStore (nullSnapshotStore)
import qualified Data.CQRS.Repository as R
import           Data.CQRS.Types.PersistedEvent
import           Data.CQRS.Types.ArchiveStore (ArchiveStore, enumerateAllEvents)
import           Network.Wai.EventSource (ServerEvent(..))
import qualified System.IO.Streams as Streams
import           System.IO.Streams (InputStream)
import           System.Random (randomIO)
import           Web.Scotty (scotty)

import           CQRSExample.TaskId (TaskId)
import           CQRSExample.AggregateAction (aggregateAction)
import           CQRSExample.Events
import           CQRSExample.Notifications
import           CQRSExample.Query
import           CQRSExample.Routing

-- Drain an input stream, applying an IO action to each element.
drain :: InputStream a -> (a -> IO ()) -> IO ()
drain inputStream f = go
  where
    go = do
      a <- Streams.read inputStream
      case a of
        Just a' -> f a' >> go
        Nothing -> return ()


-- Source of refresh events to the browser. Never returns.
eventSourcingThread :: TVar QueryState -> ArchiveStore TaskId Event -> TChan ServerEvent -> TChan (TaskId, [PersistedEvent Event]) -> IO ()
eventSourcingThread qs archiveStore serverEvents publishedEvents = do
  -- Start two threads. One thread just periodically traverses the
  -- whole archive set once in a while. For production you would want
  -- to track which archives have already been completely processed by
  -- the Query state and stop traversal at that point. The other
  -- thread just processes events published by the repository. This
  -- latter thread never terminates unless an exception is thrown.
  void $ concurrently traverseArchives processPublishedEvents
  where
    traverseArchives :: IO ()
    traverseArchives = do
      putStrLn "Traversing archives..."
      -- Traverse and process all the events.
      enumerateAllEvents archiveStore $ \inputStream -> do
        drain inputStream $ \(aggregateId, event) -> do
          processEvents aggregateId [event]
      -- Go again after a while.
      threadDelay 30000000
      traverseArchives

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
  archiveStore <- newArchiveStore randomIO storage

  -- Create the resository
  let repositorySettings = R.setSnapshotFrequency 10 $ R.defaultSettings
  let repository = R.newRepository repositorySettings aggregateAction eventStore nullSnapshotStore publishEvents

  -- Start sourcing events.
  void $ forkIO $ do
    eventSourcingThread qState archiveStore serverEvents publishedEvents

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
