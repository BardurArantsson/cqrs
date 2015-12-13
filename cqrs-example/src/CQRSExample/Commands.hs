module CQRSExample.Commands
       ( archiveCompletedTasks
       , createTask
       , completeTask
       , reopenTask
       ) where

import Control.Concurrent.STM.TVar (TVar)
import Control.Monad (forM_)
import Control.Monad.Trans.Class (lift)
import Data.CQRS.Command (createAggregate, updateAggregate, publishEvent, CommandT)
import Data.Text (Text)

import CQRSExample.Aggregates
import CQRSExample.Events
import CQRSExample.TaskId (TaskId, freshTaskId)
import CQRSExample.Query (QueryState, runQuery, qCompletedTaskIdList)

-- Shorthand.
type TaskCommandT = CommandT TaskId Task Event

-- Create new task in a project.
createTask :: Text -> TaskCommandT IO TaskId
createTask title = do
  taskId <- freshTaskId
  createAggregate taskId $ \_ -> do
    publishEvent $ TaskEvent $ TaskAdded title
    return taskId

-- Complete a task.
completeTask :: TaskId -> TaskCommandT IO (Maybe ())
completeTask taskId = updateAggregate taskId $ \get -> do
  task <- get
  case taskStatus task of
    TaskStatusOpen     -> publishEvent $ TaskEvent $ TaskCompleted
    TaskStatusComplete -> return ()
    TaskStatusArchived -> return ()

-- Reopen a task.
reopenTask :: TaskId -> TaskCommandT IO (Maybe ())
reopenTask taskId = updateAggregate taskId $ \get -> do
  task <- get
  case taskStatus task of
    TaskStatusOpen     -> return ()
    TaskStatusComplete -> publishEvent $ TaskEvent $ TaskReopened
    TaskStatusArchived -> return ()

-- Archive all completed tasks.
archiveCompletedTasks :: TVar QueryState -> TaskCommandT IO ()
archiveCompletedTasks qs = do
  -- Get a list of all the completed tasks
  taskIds <- lift $ runQuery qs qCompletedTaskIdList
  -- Go through and archive each of the tasks
  forM_ taskIds $ \taskId -> do
    updateAggregate taskId $ \get -> do
      task <- get
      case taskStatus task of
        TaskStatusOpen     -> return ()
        TaskStatusComplete -> publishEvent $ TaskEvent $ TaskArchived
        TaskStatusArchived -> return ()
