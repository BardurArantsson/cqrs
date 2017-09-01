{-# LANGUAGE DeriveGeneric #-}
module CQRSExample.Query
       ( QueryM
       , QueryState
       , QTaskState(..)
       , newQueryState
       , runQuery
       , qCompletedTaskIdList
       , qTaskList
       , reactToEvents
       ) where

import           Control.Concurrent.STM (atomically)
import           Control.Concurrent.STM.TVar (TVar, readTVar, writeTVar)
import           Control.DeepSeq (NFData(..), ($!!))
import           Control.Monad (forM_, when)
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import           Data.CQRS.Types.PersistedEvent (PersistedEvent(..))
import           Data.Int (Int32)
import qualified Data.List as L
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Text (Text)
import           Data.Typeable (Typeable)
import           GHC.Generics (Generic)

import           CQRSExample.TaskId (TaskId)
import           CQRSExample.Events (Event(..), TaskEvent(..))

-- Tasks.
data QTask =
    QTask { qTaskId :: TaskId
          , qTaskTitle :: Text
          , qTaskState :: QTaskState }
    deriving (Typeable, Ord, Eq, Show, Generic)

data QTaskState = QTaskOpen
                | QTaskCompleted
                | QTaskArchived
                  deriving (Typeable, Ord, Eq, Show, Generic)

-- Queryable state.
data QueryState = QueryState
    { qTasks :: Map TaskId QTask
    , qAggregateVersions :: Map TaskId Int32
    } deriving (Show, Generic)

-- Need NFData instances for deep evaluation.
instance NFData QTaskState
instance NFData QTask
instance NFData QueryState

-- Query monad.
type QueryM a = ReaderT (TVar QueryState) IO a

-- Create query state.
newQueryState :: QueryState
newQueryState = QueryState { qTasks = M.empty
                           , qAggregateVersions = M.empty
                           }

-- runQuery
runQuery :: TVar QueryState -> QueryM a -> IO a
runQuery qs q = runReaderT q qs

-- Run query against the current state and return the result.
qRunQ :: QueryM QueryState
qRunQ = do
  qsr <- ask
  lift $ atomically $ readTVar qsr

-- Query the task list
qTaskList :: QueryM [(TaskId, Text, QTaskState)]
qTaskList = do
  tasks <- fmap qTasks qRunQ
  return $ map f $ filter p $ L.sortBy sf $ M.elems tasks
  where
    f t = (qTaskId t, qTaskTitle t, qTaskState t)
    sf (QTask _ title1 _) (QTask _ title2 _) = compare title1 title2
    p (QTask _ _ QTaskOpen) = True
    p (QTask _ _ QTaskCompleted) = True
    p (QTask _ _ QTaskArchived) = False

qCompletedTaskIdList :: QueryM [TaskId]
qCompletedTaskIdList = do
  tasks <- fmap qTasks qRunQ
  return $ map f $ filter p $ M.elems tasks
    where
      f (QTask i _ _) = i
      p (QTask _ _ QTaskCompleted) = True
      p (QTask _ _ QTaskOpen) = False
      p (QTask _ _ QTaskArchived) = False

reactToEvents :: TaskId -> [PersistedEvent Event] -> QueryM ()
reactToEvents aggregateId evs =
  -- Just handle each event separately; in real production code
  -- updates should probably be batched to avoid excessive IO
  -- operations.
  forM_ evs $ \pev -> do
    -- Log event data.
    lift $ putStrLn $ "aggregateId: " ++ show aggregateId ++ " event: " ++ show pev
    -- Update the query state
    qsVar <- ask
    lift $ atomically $ do
      qs <- readTVar qsVar
      when (isApplicable qs pev) $ do
        let qs' = react (peEvent pev) $!! qs
        let qs'' = updateAggregateVersion qs' pev
        writeTVar qsVar $!! qs''

  where
    -- Check if the event is actually next in the sequence. If it
    -- isn't then we have a race between an event which arrived via
    -- repository publishing and an event which was read from the
    -- persistent event store archives. In this case we just ignore
    -- the event since either a) we've already processed it, or b)
    -- it's a future event which we cannot quite process yet. We'll
    -- eventually re-receive the event via archive traversal.
    isApplicable :: QueryState -> PersistedEvent a -> Bool
    isApplicable qs pev =
        peSequenceNumber pev == 1 + M.findWithDefault (-1) aggregateId (qAggregateVersions qs)

    updateAggregateVersion :: QueryState -> PersistedEvent a -> QueryState
    updateAggregateVersion qs pev =
        qs { qAggregateVersions = M.insert aggregateId (peSequenceNumber pev) $ qAggregateVersions qs }

    reactTask (TaskAdded title) =
        M.insert aggregateId (QTask aggregateId title QTaskOpen)
    reactTask TaskCompleted =
        M.adjust (\task -> task { qTaskState = QTaskCompleted }) aggregateId
    reactTask TaskReopened =
        M.adjust (\task -> task { qTaskState = QTaskOpen }) aggregateId
    reactTask TaskArchived =
        M.adjust (\task -> task { qTaskState = QTaskArchived }) aggregateId

    react (TaskEvent e) qs =
        qs { qTasks = reactTask e $ qTasks qs }
