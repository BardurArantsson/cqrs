module CQRSExample.AggregateAction
       ( aggregateAction
       ) where

import           Data.CQRS.Command (AggregateAction)

import           CQRSExample.Aggregates
import           CQRSExample.Events

aggregateAction :: AggregateAction Task Event
aggregateAction mTask (TaskEvent taskEvent) = handleCreate taskEvent
      where
        -- Handle the creation event specially
        handleCreate (TaskAdded title) =
            case mTask of
              Nothing -> newTask title
              Just _  -> error "Cannot add task which already exists"
        handleCreate event =
            case mTask of
              Nothing -> error "Cannot update task which doesn't exist"
              Just task -> handleUpdate event task
        -- Handle the "update" case
        handleUpdate (TaskAdded _) _ = error "TaskAdded reached handleUpdate"
        handleUpdate TaskCompleted task = task { taskStatus = TaskStatusComplete }
        handleUpdate TaskReopened task = task { taskStatus = TaskStatusOpen }
        handleUpdate TaskArchived task = task { taskStatus = TaskStatusArchived }
