{-# LANGUAGE DeriveGeneric #-}
module CQRSExample.Events
       ( Event(..)
       , TaskEvent(..)
       ) where

import           Control.DeepSeq (NFData(..))
import           Data.Serialize (Serialize(..), putWord8, getWord8)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Typeable (Typeable)
import           GHC.Generics (Generic)

data Event = TaskEvent TaskEvent
           deriving (Typeable, Show, Generic)

data TaskEvent = TaskAdded Text
               | TaskCompleted
               | TaskReopened
               | TaskArchived
               deriving (Typeable, Show, Generic)

-- NFData instances.
instance NFData TaskEvent
instance NFData Event

-- Serialize instances. We use manual serialization here since it is
-- extremely important that events remain forward-compatible for
-- (essentially) all time. You could also use e.g. Protocol Buffers
-- and be very careful about only expanding the schema in
-- backward-compatible ways.
instance Serialize TaskEvent where
    put (TaskAdded t) = do
      putWord8 1
      put $ T.unpack t
    put TaskCompleted = do
      putWord8 2
    put TaskReopened = do
      putWord8 3
    put TaskArchived = do
      putWord8 4
    get = do
      i <- getWord8
      case i of
        1 -> fmap (TaskAdded . T.pack) get
        2 -> return TaskCompleted
        3 -> return TaskReopened
        4 -> return TaskArchived
        _ -> fail $ "Unrecognized TaskEvent tag value" ++ show i

instance Serialize Event where
    put (TaskEvent taskEvent) = do
      putWord8 1 -- Marker for future expansion
      put $ taskEvent
    get = do
      i <- getWord8
      case i of
        1 -> fmap TaskEvent get
        _ -> fail $ "Unrecognized TaskEvent tag value: " ++ show i
