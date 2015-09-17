module Data.CQRS.Test.Internal.ArchiveStoreUtils
    ( readAllEventsFromArchiveStore
    ) where

import           Control.Monad.IO.Class (liftIO)
import           Data.IORef (newIORef, atomicModifyIORef', readIORef)
import           Data.CQRS.Types.ArchiveMetadata (ArchiveMetadata(..))
import           Data.CQRS.Types.ArchiveRef (ArchiveRef(..))
import           Data.CQRS.Types.ArchiveStore (ArchiveStore(..), enumerateAllEvents)
import           Data.CQRS.Types.PersistedEvent (PersistedEvent)
import           Data.CQRS.Test.Internal.Scope (ScopeM, ask)
import           Data.UUID.Types (UUID)
import qualified System.IO.Streams.List as SL
import           Test.Hspec (shouldBe)

-- Read all events from the event store, using the archives. Also
-- asserts that all archives have consistent metadata and returns the
-- number of archives.
readAllEventsFromArchiveStore :: (s -> ArchiveStore e) -> ScopeM s (Int, [(UUID, PersistedEvent e)])
readAllEventsFromArchiveStore f = do
  archiveStore <- fmap f ask
  archiveCount <- liftIO $ assertConsistentArchiveMetadata archiveStore
  events <- liftIO $ readAllEvents' archiveStore
  return (archiveCount, events)

-- Retrieve all events from an event store.
readAllEvents' :: ArchiveStore e -> IO [(UUID, PersistedEvent e)]
readAllEvents' archiveStore = do
  eventsRef <- newIORef [ ]
  enumerateAllEvents archiveStore $ \inputStream -> do
    events <- SL.toList inputStream
    atomicModifyIORef' eventsRef (\events' -> (events' ++ events, ()))
  readIORef eventsRef

-- Assert that all archives have consistent metadata and return the
-- number of archives that were traversed (not counting the "Current"
-- archive).
assertConsistentArchiveMetadata :: ArchiveStore e -> IO Int
assertConsistentArchiveMetadata archiveStore = do
  -- We have a special case for "no archives".
  maybeLatestArchiveMetadata <- asReadLatestArchiveMetadata archiveStore
  case maybeLatestArchiveMetadata of
    Nothing ->
      return 0  -- There's only the "current" archive, and so there's nothing to verify.
    Just archiveMetadata ->
      walk CurrentArchive archiveMetadata 0

  where
    walk previousArchiveRef archiveMetadata count = do
      -- Make sure the nextArchiveId reference fits with the previous (recursion-wise) archive.
      previousArchiveRef `shouldBe` amNextArchiveId archiveMetadata
      -- Load up the previous archive in the chain.
      case amPreviousArchiveId archiveMetadata of
        Nothing -> do
          return $ count + 1 -- Reached the first archive.
        Just previousArchiveId -> do
          -- Load up metadata for previous archive.
          maybePreviousArchiveMetadata <- (asReadArchiveMetadata archiveStore) previousArchiveId
          case maybePreviousArchiveMetadata of
            Nothing ->
              error "No metadata for previous archive?"
            Just previousArchiveMetadata -> do
              -- Archive ID of the current archive
              let archiveRef = NamedArchive $ amArchiveId archiveMetadata
              -- Make sure we the next link of the previous archive point to "us".
              archiveRef `shouldBe` (amNextArchiveId previousArchiveMetadata)
              -- Check the remainder of the chain of archives.
              walk archiveRef previousArchiveMetadata $ count + 1
