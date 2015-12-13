{-# LANGUAGE DeriveGeneric #-}
module CQRSExample.Aggregates
       ( newTask
       , Task(..)
       , TaskStatus(..)
       ) where

import           Control.DeepSeq (NFData(..))
import           Control.Monad (unless)
import           Data.Serialize (Serialize(..), getWord8, putWord8)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Typeable (Typeable)
import           GHC.Generics (Generic)

data TaskStatus = TaskStatusOpen
                | TaskStatusComplete
                | TaskStatusArchived
                  deriving (Typeable, Eq, Show, Generic)

data Task = Task
    { taskTitle :: Text
    , taskStatus :: TaskStatus
    } deriving (Typeable, Eq, Show, Generic)

newTask :: Text -> Task
newTask title = Task title TaskStatusOpen

-- Serialize instances for snapshots. In the case of snapshots,
-- compatibility is not that important and de-serialization can fail
-- without huge host, i.e. the aggregate can always be reconstructed
-- from events rather than using the snapshot. It *is* important that
-- a format change is detected, though, which is why we're putting in
-- a "version" marker.

instance Serialize TaskStatus where
    put status = do
      putWord8 1 -- Version number.
      case status of
        TaskStatusOpen -> putWord8 1
        TaskStatusComplete -> putWord8 2
        TaskStatusArchived -> putWord8 3
    get = do
      v <- getWord8 -- Version number.
      unless (v == 1) $ fail "Not the correct version"
      i <- getWord8
      case i of
        1 -> return TaskStatusOpen
        2 -> return TaskStatusComplete
        3 -> return TaskStatusArchived
        _ -> error "Versioning inconsistency; this is BAD"

instance Serialize Task where
    put (Task t1 t2) = do
      putWord8 1 -- Version marker.
      put $ T.unpack t1
      put t2
    get = do
      v <- getWord8 -- Version number
      unless (v == 1) $ fail "Not the correct version"
      t1 <- fmap T.pack get
      t2 <- get
      return $ Task t1 t2

-- NFData instances.
instance NFData TaskStatus
instance NFData Task
