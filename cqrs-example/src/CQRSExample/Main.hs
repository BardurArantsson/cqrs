module Main ( main ) where

import           Control.Concurrent (forkIO)
import           Control.Concurrent.STM (atomically, TChan)
import qualified Control.Concurrent.STM.TChan as C
import           Control.Concurrent.STM.TVar (TVar, newTVarIO)
import           Control.Monad (void, forever, when)
import           Data.CQRS.Memory (newEventStore, newStorage)
import           Data.CQRS.SnapshotStore (nullSnapshotStore)
import qualified Data.CQRS.Repository as R
import           Data.CQRS.Types.PersistedEvent
import           Network.Wai.EventSource (ServerEvent(..))
import           Web.Scotty (scotty)

import           CQRSExample.TaskId (TaskId)
import           CQRSExample.AggregateAction (aggregateAction)
import           CQRSExample.Events
import           CQRSExample.Notifications
import           CQRSExample.Query
import           CQRSExample.Routing

-- Source of refresh events to the browser. Never returns.
eventSourcingThread :: TVar QueryState -> TChan ServerEvent -> TChan (TaskId, [PersistedEvent Event]) -> IO ()
eventSourcingThread qs serverEvents publishedEvents = do
    processPublishedEvents
  where
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

  -- Create the resository
  let repositorySettings = R.setSnapshotFrequency 10 $ R.defaultSettings
  let repository = R.newRepository repositorySettings aggregateAction eventStore nullSnapshotStore publishEvents

  -- Start sourcing events.
  void $ forkIO $ do
    eventSourcingThread qState serverEvents publishedEvents

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
