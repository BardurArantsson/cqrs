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

data Event i e =
    Event { eAggregateId :: i
          , ePersistedEvent :: PersistedEvent e
          , eArchiveRef :: ArchiveRef
          }
    deriving (Show)

data Store i e = Store
    { msEvents :: Seq (Event i e)
      -- Archives: Archives are kept in chronological order; first to last.
    , msArchives :: Seq ArchiveMetadata
    }

-- | Storage used for memory-backed EventStore and ArchiveStore.
newtype Storage i e = Storage (TVar (Store i e))

-- | Create backing memory for a memory-based event store
-- or archive store.
newStorage :: IO (Storage i e)
newStorage = atomically $ fmap Storage $ newTVar $ Store S.empty S.empty
