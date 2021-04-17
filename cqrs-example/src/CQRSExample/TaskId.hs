{-# LANGUAGE DeriveGeneric #-}
module CQRSExample.TaskId
    ( TaskId
    , freshTaskId
    ) where

import           Control.DeepSeq (NFData(..))
import           Control.Monad (replicateM)
import           Control.Monad.IO.Class (MonadIO(liftIO))
import           Data.Aeson.Types (ToJSON(..), Value(String))
import           Data.Serialize (Serialize(..))
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import           GHC.Generics (Generic)
import           System.Random (randomRIO)
import           Web.Scotty (Parsable(..))

-- Task identifier.
newtype TaskId = TaskId Text
    deriving (Show, Ord, Eq, Generic)

instance NFData TaskId

-- Conversion to JSON.
instance ToJSON TaskId where
    toJSON (TaskId tid) = String tid

-- Serialization support.
instance Serialize TaskId where
    put (TaskId t) = do
      put $ T.unpack t
    get = do
      fmap (TaskId . T.pack) get

-- Create a fresh task ID.
freshTaskId :: MonadIO m => m TaskId
freshTaskId = fmap (TaskId . T.pack) $ replicateM 16 $ liftIO $ randomRIO ('a', 'z')

-- Need parsing for TaskIds
instance Parsable TaskId where
    parseParam text =
      Right $ TaskId $ TL.toStrict text
