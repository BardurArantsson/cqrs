{-# LANGUAGE OverloadedStrings #-}
module CQRSExample.Routing
       ( toServerEvent
       , routes
       ) where

import           Blaze.ByteString.Builder.ByteString (fromLazyByteString)
import           Control.Monad (void)
import           Control.Monad.IO.Class (liftIO)
import           Control.Concurrent.STM (atomically, TChan)
import           Control.Concurrent.STM.TChan (dupTChan, readTChan)
import           Control.Concurrent.STM.TVar (TVar)
import           Data.Aeson.Types (ToJSON(..), Value(..))
import           Data.Aeson (encode)
import           Data.CQRS.Command (Repository, runCommandT)
import           Network.Wai.EventSource (ServerEvent(..))
import           Network.Wai.EventSource.EventStream (eventToBuilder)
import           Network.Wai.Middleware.Static (staticPolicy, addBase)
import           Web.Scotty (ScottyM, get, stream, setHeader, post, json, middleware, param, redirect)

import           CQRSExample.Aggregates (Task)
import qualified CQRSExample.Commands as C
import           CQRSExample.Events
import           CQRSExample.Json
import           CQRSExample.Query (QueryState)
import           CQRSExample.TaskId (TaskId)

-- Convert a JSON value to a server event.
toServerEvent :: ToJSON j => j -> ServerEvent
toServerEvent j = ServerEvent Nothing Nothing builders
    where builders = [ fromLazyByteString $ encode j ]

-- Routes for the example application.
routes :: TVar QueryState -> Repository TaskId Task Event -> TChan ServerEvent -> ScottyM ()
routes qs repository serverEvents = do
  -- Middleware to serve static files
  middleware $ staticPolicy (addBase "static")

  -- Redirect to index
  get "/" $
    redirect "/index.html"

  -- Actions
  post "/tasks/archive-completed" $
    run $ C.archiveCompletedTasks qs

  post "/tasks/complete" $ do
    tid <- param "id"
    run $ C.completeTask tid

  post "/tasks/reopen" $ do
    tid <- param "id"
    run $ C.reopenTask tid

  get "/tasks" $ do
    tasks <- liftIO $ qTaskListJson qs
    json tasks

  post "/tasks" $ do
    title <- param "title"
    run $ C.createTask title

  -- Notifications
  get "/events" $ do
    setHeader "Content-Type" "text/event-stream"
    stream $ \sendChunk flush -> do
      chan <- atomically $ dupTChan serverEvents
      loop sendChunk flush chan

  where
    loop sendChunk flush chan = do
      e <- atomically $ readTChan chan
      case eventToBuilder e of
        Nothing -> return () -- Done
        Just builder -> do
            void $ sendChunk builder
            void flush
            loop sendChunk flush chan

    run command = do
      void $ liftIO $ runCommandT repository command
      json Null
