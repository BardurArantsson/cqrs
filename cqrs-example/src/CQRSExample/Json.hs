{-# LANGUAGE OverloadedStrings #-}
module CQRSExample.Json
       ( qTaskListJson
       ) where

import           Control.Concurrent.STM.TVar (TVar)
import           Control.Monad (liftM)
import           Data.Aeson (ToJSON, Value)
import           Data.Aeson.Types (object, Value(..))
import           Data.Text.Encoding (decodeUtf8)
import           Data.UUID.Types (toASCIIBytes)

import           CQRSExample.Query

-- Generate a list of JSONable values by mapping a function on a query.
qListToJson :: ToJSON b => (QueryM [a]) -> (a -> b) -> TVar QueryState -> IO [b]
qListToJson q f p = liftM (map f) $ runQuery p q

-- Query the task list, producing JSON.
qTaskListJson :: TVar QueryState -> IO [Value]
qTaskListJson qs = qListToJson qTaskList f qs
    where
      f (tid, title, state) =
          object [ ("id", String $ decodeUtf8 $ toASCIIBytes tid)
                 , ("title", String title)
                 , ("done", Bool $ isDone state) ]
      isDone QTaskOpen = False
      isDone QTaskCompleted = True
      isDone QTaskArchived = True
