{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}
module Data.CQRS.Types.ArchiveStore
       ( ArchiveRef
       , ArchiveMetadata(..)
       , ArchiveStore(..)
       , StoreError(..)
       , transform
       , enumerateAllEvents
       , rotateArchives
       ) where

import           Control.Monad ((>=>), when, void)
import           Data.CQRS.Types.ArchiveRef
import           Data.CQRS.Types.ArchiveMetadata
import           Data.CQRS.Types.Iso
import           Data.CQRS.Types.PersistedEvent
import           Data.CQRS.Types.StoreError
import           Data.UUID.Types (UUID)
import           System.IO.Streams (InputStream)
import qualified System.IO.Streams.Combinators as SC

-- | ArchiveStore for events of type e.
data ArchiveStore i e = ArchiveStore {
      -- | 'esGetUnarchivedEventCount` returns the number of currently
      -- unarchived events. Because other processes/threads may be
      -- accessing the event store concurrently, the returned value
      -- may only be treated as an estimate.  The caller is
      -- guaranteed, however, that -- absent any other process/thread
      -- calling 'esArchiveEvents' -- the value will be monotonically
      -- increasing.
      asGetUnarchivedEventCount :: IO Int
    ,
      -- | 'esArchiveEvents n' archives up to 'n' events. This
      -- function does nothing if 'n' is less than or equal to 0.  If
      -- there are fewer than 'n' unarchived events, then all those
      -- events will be archived. Returns the UUID of the newly
      -- created archive, if any.
      asArchiveEvents :: Int -> IO (Maybe UUID)
    ,
      -- | Retrieve the metadata of the latest archive. Returns
      -- Returns `Nothing` if there are no archives.
      asReadLatestArchiveMetadata :: IO (Maybe ArchiveMetadata)
    ,
      -- | Retrieve archive metadata for a specified archive.
      asReadArchiveMetadata :: UUID -> IO (Maybe ArchiveMetadata)
    ,
      -- | Read all the events in an archive. There's no guarantee on
      -- the ordering of the returned events except that the events
      -- for any specific aggregate root are returned in order of
      -- incresing version number.
      asReadArchive :: forall a . ArchiveRef -> (InputStream (i, PersistedEvent e) -> IO a) -> IO a
    }

-- | Transform an archive store via an isomorphism for the events and
-- and aggregate IDs. This can be used to add
-- serialization/deserialization to event stores which do not support
-- storing anything other than binary data.
transform :: forall e e' i i' . Iso e' e -> Iso i' i -> ArchiveStore i e -> ArchiveStore i' e'
transform (_, g) (_, gi) (ArchiveStore getUnarchivedEventCount archiveEvents readLatestArchiveMetadata' readArchiveMetadata' readArchive') =
    ArchiveStore getUnarchivedEventCount archiveEvents readLatestArchiveMetadata' readArchiveMetadata' readArchive
  where
    readArchive :: ArchiveRef -> (InputStream (i', PersistedEvent e') -> IO a) -> IO a
    readArchive archiveRef p = readArchive' archiveRef $ SC.map (\(i, e) -> (gi i, fmap g e)) >=> p

-- | Enumerate all events in all archives. __This should ONLY be used
-- for debugging purposes.__
enumerateAllEvents :: ArchiveStore i e -> (InputStream (i, PersistedEvent e) -> IO ()) -> IO ()
enumerateAllEvents (ArchiveStore _ _ readLatestArchiveMetadata readArchiveMetadata readArchive) p =
  findFirstArchive >>= rollForward
  where
    readArchiveMetadata' archiveId =
        readArchiveMetadata archiveId >>= \x -> case x of
          Just archiveMetadata ->
            return archiveMetadata
          Nothing ->
            error $ "Archive inconsistency; archive reference chain contains non-existent archive " ++ show archiveId

    rollForward archiveRef = do
      readArchive archiveRef p
      case archiveRef of
        CurrentArchive         -> return ()
        NamedArchive archiveId -> readArchiveMetadata' archiveId >>= rollForward . amNextArchiveId

    findFirstArchive =
      readLatestArchiveMetadata >>= \x -> case x of
        Nothing              -> return CurrentArchive
        Just archiveMetadata -> search archiveMetadata
      where
        search archiveMetadata =
          case amPreviousArchiveId archiveMetadata of
            Nothing                -> return $ NamedArchive $ amArchiveId archiveMetadata
            Just previousArchiveId -> readArchiveMetadata' previousArchiveId >>= search

-- | Perform event archival until the current number of unarchived
-- events goes below the given 'archiveSize'. Does nothing if the
-- given 'archiveSize' is not a positive number.
rotateArchives :: ArchiveStore i e -> Int -> IO ()
rotateArchives eventStore archiveSize =
    if archiveSize > 0 then
        loop
      else
        return ()
  where
    loop = do
      -- Count number of unarchived events
      count <- asGetUnarchivedEventCount eventStore
      -- Do we have enough to fill an archive?
      when (count >= archiveSize) $ do
        -- Archive an archive's worth of events.
        void $ asArchiveEvents eventStore $ archiveSize
        -- Try again to see if there's more we can archive.
        loop
