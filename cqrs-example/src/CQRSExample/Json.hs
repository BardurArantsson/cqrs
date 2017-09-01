{-# LANGUAGE OverloadedStrings #-}
module CQRSExample.Json
       ( qTaskListJson
       ) where

import           Control.Applicative ((<$>))
import           Control.Concurrent.STM.TVar (TVar)
import           Data.Aeson (ToJSON(..), Value)
import           Data.Aeson.Types (object, Value(..))

import           CQRSExample.Query

-- Generate a list of JSONable values by mapping a function on a query.
qListToJson :: QueryM [a] -> (a -> b) -> TVar QueryState -> IO [b]
qListToJson q f p = map f <$> runQuery p q

-- Query the task list, producing JSON.
qTaskListJson :: TVar QueryState -> IO [Value]
qTaskListJson = qListToJson qTaskList f
    where
      f (tid, title, state) =
          object [ ("id", toJSON tid)
                 , ("title", String title)
                 , ("done", Bool $ isDone state) ]
      isDone QTaskOpen = False
      isDone QTaskCompleted = True
      isDone QTaskArchived = True
