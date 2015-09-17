module Data.CQRS.Memory.Internal.ArchiveStore
    ( newArchiveStore
    ) where

import           Control.Concurrent.STM (STM, atomically)
import           Control.Concurrent.STM.TVar (readTVar, readTVarIO, writeTVar)
import           Control.Monad (liftM)
import           Data.CQRS.Types.ArchiveRef
import           Data.CQRS.Types.ArchiveMetadata
import           Data.CQRS.Types.ArchiveStore (ArchiveStore(..))
import           Data.CQRS.Types.PersistedEvent (PersistedEvent(..))
import           Data.CQRS.Memory.Internal.Storage
import qualified Data.Foldable as F
import           Data.Sequence (Seq, ViewR(..), ViewL(..), viewr, viewl, (><), (|>))
import qualified Data.Sequence as S
import           Data.UUID.Types (UUID)
import           System.IO.Streams (InputStream)
import qualified System.IO.Streams.List as SL

lastOption :: Seq a -> Maybe a
lastOption xs = case viewr xs of
                  EmptyR   -> Nothing
                  (_ :> x) -> Just $ x

uncons :: Seq a -> Maybe (a, Seq a)
uncons s = case viewl s of
             EmptyL    -> Nothing
             (x :< xs) -> Just (x, xs)

inCurrentArchive :: Event e -> Bool
inCurrentArchive (Event _ _ CurrentArchive) = True
inCurrentArchive (Event _ _ (NamedArchive _)) = False

readLatestArchiveMetadata :: Storage e -> IO (Maybe ArchiveMetadata)
readLatestArchiveMetadata (Storage store) = do
  archives <- fmap msArchives $ readTVarIO store
  return $ lastOption archives

readArchiveMetadata :: Storage e -> UUID -> IO (Maybe ArchiveMetadata)
readArchiveMetadata (Storage store) archiveId = do
  archives <- fmap msArchives $ readTVarIO store
  return $ F.find (\a -> amArchiveId a == archiveId) archives

readArchive :: Storage e -> ArchiveRef -> (InputStream (UUID, PersistedEvent e) -> IO a) -> IO a
readArchive (Storage store) archiveRef f = do
  events <- liftM msEvents $ readTVarIO store
  eventStream <- SL.fromList $ F.toList
                             $ fmap (\e -> (eAggregateId e, ePersistedEvent e))
                             $ S.filter (\e -> archiveRef == eArchiveRef e)
                             $ events
  f eventStream

getUnarchivedEventCount :: Storage e -> IO Int
getUnarchivedEventCount (Storage store) = atomically getCurrentEventCount
  where
    getCurrentEventCount :: STM Int
    getCurrentEventCount = do
      events <- fmap msEvents $ readTVar store
      return $ S.length $ S.filter inCurrentArchive events

archiveEvents :: Storage e -> IO UUID -> Int -> IO (Maybe UUID)
archiveEvents (Storage store) uuidSupply archiveSize =
    if (archiveSize > 0) then
      do
        newArchiveId <- uuidSupply
        doArchiveEvents newArchiveId
        return $ Just newArchiveId
      else
        return $ Nothing
  where
    doArchiveEvents :: UUID -> IO ()
    doArchiveEvents newArchiveId = do
      atomically $ do
        memoryStorage <- readTVar store
        let archives = msArchives memoryStorage
        let events = msEvents memoryStorage
        -- Find previous archive to point to the new archive.
        let maybePreviousArchive = lastOption archives
        -- Update the last archive's metadata (if necessary).
        let archives' = case maybePreviousArchive of
                          Nothing ->
                              archives -- No changes necessary
                          Just previousArchive ->
                              let previousArchive' = previousArchive { amNextArchiveId = NamedArchive newArchiveId } in
                              S.update (S.length archives - 1) previousArchive' archives
        -- Create new archive
        let newArchiveMetadata = ArchiveMetadata
               { amArchiveId = newArchiveId
               , amPreviousArchiveId = fmap amArchiveId maybePreviousArchive
               , amNextArchiveId = CurrentArchive
               }
        let archives'' = archives' |> newArchiveMetadata
        -- Move <archiveSize'> events in Current to new archive. We
        -- exploit the fact that events are added in chronological
        -- order.
        let (archivedEvents, unarchivedEvents) = S.partition (not . inCurrentArchive) events
        let (archivedEvents', unarchivedEvents') = archive archiveSize newArchiveId archivedEvents unarchivedEvents
        let events' = archivedEvents' >< unarchivedEvents'
        -- Actually perform the update
        writeTVar store $ memoryStorage { msArchives = archives''
                                        , msEvents = events' }

    archive :: Int -> UUID -> Seq (Event e) -> Seq (Event e) -> (Seq (Event e), Seq (Event e))
    archive 0 _            archivedEvents unarchivedEvents = (archivedEvents, unarchivedEvents)
    archive _ _            archivedEvents unarchivedEvents | S.null unarchivedEvents = (archivedEvents, unarchivedEvents)
    archive n newArchiveId archivedEvents unarchivedEvents =
        -- Take off the first unarchived event
        let (unarchivedEvent, unarchivedEvents') = maybe (error "unarchivedEvents unexpectedly empty") id (uncons unarchivedEvents) in
        -- Archive it
        let archivedEvent = unarchivedEvent { eArchiveRef = NamedArchive newArchiveId } in
        -- Bundle up
        archive (n - 1) newArchiveId (archivedEvents |> archivedEvent) unarchivedEvents'

-- | Create a memory-backed archive store.
newArchiveStore :: Show e => IO UUID -> Storage e -> IO (ArchiveStore e)
newArchiveStore uuidSupply storage = do
  return $ ArchiveStore
    { asGetUnarchivedEventCount = getUnarchivedEventCount storage
    , asArchiveEvents = archiveEvents storage uuidSupply
    , asReadLatestArchiveMetadata = readLatestArchiveMetadata storage
    , asReadArchiveMetadata = readArchiveMetadata storage
    , asReadArchive = readArchive storage
    }
