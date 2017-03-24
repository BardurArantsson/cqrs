{-# LANGUAGE OverloadedStrings #-}
module CQRSExample.Notifications
    ( updateNotifications
    , Notifications
    ) where

import           Data.Aeson (ToJSON(..), object, (.=))
import           Data.CQRS.Types.PersistedEvent
import qualified Data.Foldable as F
import           Data.Monoid ((<>))
import           Data.Sequence (Seq)
import qualified Data.Sequence as S
import           Data.Text (Text)

import           CQRSExample.TaskId (TaskId)
import           CQRSExample.Events (Event(..), TaskEvent(..))

-- The available types of individual notifications.
data Notification =
    AddedTask Text
  | ArchivedTask
  | Refresh
    deriving (Show, Eq, Ord)

-- Convert a notification to JSON
instance ToJSON Notification where
    toJSON (AddedTask title) =
        object [ "type" .= ("added-task" :: Text)
               , "title" .= title ]
    toJSON ArchivedTask =
        object [ "type" .= ("archived-task" :: Text)
               ]
    toJSON Refresh =
        object [ "type" .= ("refresh" :: Text)
               ]

-- Notifications that have been gathered up. Forms a monoid whose
-- operator corresponds to the aggregation of notifications.
data Notifications = Notifications (Seq Notification)
  deriving (Eq, Show)

instance ToJSON Notifications where
    toJSON (Notifications ns) =
        object [ "notifications" .= (toJSON $ F.toList ns) ]

instance Monoid Notifications where
    mempty = Notifications mempty
    mappend (Notifications ns) (Notifications ns') = Notifications (ns <> ns')

-- Update a Notifications value to account for the given persisted events.
-- Ideally, we'd coalesce multiple notifications of the same types, but for
-- this example it's not really necessary so we'll just keep it simple.
updateNotifications :: (TaskId, [PersistedEvent TaskId Event]) -> Notifications -> Notifications
updateNotifications (_, publishedEvents) (Notifications notifications) =
  Notifications $ notifications <> F.foldMap (S.fromList . f . peEvent) publishedEvents
  where
    f (TaskEvent (TaskAdded title)) =
        [ AddedTask title, Refresh ]
    f (TaskEvent TaskCompleted) =
        [ Refresh ]
    f (TaskEvent TaskReopened) =
        [ Refresh ]
    f (TaskEvent TaskArchived) =
        [ ArchivedTask, Refresh ]
