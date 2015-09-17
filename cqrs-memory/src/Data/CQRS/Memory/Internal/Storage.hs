module Data.CQRS.Memory.Internal.Storage
    ( Event(..)
    , Store(..)
    , Storage(..)
    , newStorage
    ) where

import           Control.Concurrent.STM (atomically)
import           Control.Concurrent.STM.TVar (TVar, newTVar)
import           Data.CQRS.Types.ArchiveRef
import           Data.CQRS.Types.ArchiveMetadata
import           Data.CQRS.Types.PersistedEvent (PersistedEvent(..))
import           Data.Sequence (Seq)
import qualified Data.Sequence as S
import           Data.UUID.Types (UUID)

data Event e =
    Event { eAggregateId :: UUID
          , ePersistedEvent :: PersistedEvent e
          , eArchiveRef :: ArchiveRef
          }
    deriving (Show)

data Store e = Store
    { msEvents :: Seq (Event e)
      -- Archives: Archives are kept in chronological order; first to last.
    , msArchives :: Seq ArchiveMetadata
    }

-- | Storage used for memory-backed EventStore and ArchiveStore.
newtype Storage e = Storage (TVar (Store e))

-- | Create backing memory for a memory-based event store
-- or archive store.
newStorage :: IO (Storage e)
newStorage = atomically $ fmap Storage $ newTVar $ Store S.empty S.empty
