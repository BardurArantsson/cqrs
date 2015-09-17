{-# LANGUAGE OverloadedStrings #-}
module Data.CQRS.PostgreSQL.Internal.ArchiveStore
    ( newArchiveStore
    ) where

import           Control.Monad ((>=>))
import           Data.ByteString (ByteString)
import           Data.Pool (Pool, withResource)
import           Data.CQRS.Types.ArchiveRef (ArchiveRef(..))
import           Data.CQRS.Types.ArchiveStore (ArchiveStore(..), ArchiveMetadata(..))
import           Data.CQRS.Types.PersistedEvent (PersistedEvent(..))
import           Data.CQRS.PostgreSQL.Internal.Utils (execSql, ioQuery, SqlValue(..), withTransaction, badQueryResultMsg)
import           Data.UUID.Types (UUID)
import           Database.PostgreSQL.LibPQ (Connection)
import           System.IO.Streams (InputStream)
import qualified System.IO.Streams as Streams
import qualified System.IO.Streams.Combinators as SC

unpackArchiveMetadata :: [SqlValue] -> ArchiveMetadata
unpackArchiveMetadata [ SqlUUID (Just archiveId)
                      , SqlUUID maybePreviousArchiveId
                      , SqlUUID maybeNextArchiveId ] = ArchiveMetadata
  { amArchiveId = archiveId
  , amPreviousArchiveId = maybePreviousArchiveId
  , amNextArchiveId = maybe CurrentArchive NamedArchive maybeNextArchiveId
  }
unpackArchiveMetadata columns =
  error $ badQueryResultMsg [ ] columns

getUnarchivedEventCount :: Pool Connection -> IO Int
getUnarchivedEventCount cp = withResource cp $ \c -> do
    maybeCount <- withTransaction c $ do
      ioQuery c sqlUnarchivedCount [ ] $ SC.map unpackCount >=> Streams.read
    case maybeCount of
      Nothing ->
        error "Query error counting number of unarchived events"
      Just count ->
        return count
  where
    unpackCount [ SqlInt32 (Just count) ] = fromIntegral count
    unpackCount [ SqlInt64 (Just count) ] = fromIntegral count
    unpackCount columns = error $ badQueryResultMsg [ ] columns

    sqlUnarchivedCount =
        "SELECT COUNT(*) \
        \  FROM event \
        \ WHERE archive_uuid IS NULL"

archiveEvents :: Pool Connection -> IO UUID -> Int -> IO (Maybe UUID)
archiveEvents cp uuidSupply archiveSize =
  if (archiveSize > 0) then
      withResource cp $ \c -> do
        withTransaction c $ do
          archiveId <- uuidSupply
          -- Create new archive
          execSql c sqlInsertNewCurrentArchive [ SqlUUID $ Just archiveId ]
          execSql c sqlUpdatePreviousCurrentArchive [ ]
          -- Move <archiveSize> events which don't have an archive into the new archive
          execSql c sqlFillNextArchive [ SqlUUID $ Just archiveId, SqlInt32 $ Just $ fromIntegral archiveSize ]
          -- Return its ID
          return $ Just archiveId
    else
      return Nothing

  where

    sqlInsertNewCurrentArchive =
        -- Note that we use a RIGHT OUTER join here to ensure that this also works in the
        -- case where there are no pre-existing archives.
        "INSERT INTO archive (archive_uuid, prev_archive_uuid, next_archive_uuid) \
         \    SELECT $1, A.archive_uuid, NULL \
         \      FROM archive A RIGHT OUTER JOIN (SELECT 1) AS B \
         \        ON A.next_archive_uuid IS NULL"

    sqlUpdatePreviousCurrentArchive =
        -- Update the previous <current> archive to point to the new
        -- archive as its "next" archive.  This is a bit more
        -- complicated then just setting the next_archive_uuid for the
        -- archive with the next_archive_uuid=NULL because we've just
        -- inserted a new archive with next_archive_uuid=NULL so now we
        -- have TWO archives with next_archive_uuid=NULL. The nested
        -- select takes care of finding the proper row.
        "UPDATE archive A0 \
        \   SET next_archive_uuid = (SELECT archive_uuid \
        \                               FROM archive AS A1 \
        \                              WHERE A1.prev_archive_uuid = A0.archive_uuid) \
        \ WHERE next_archive_uuid IS NULL"

    sqlFillNextArchive =
      "UPDATE event \
      \         SET archive_uuid = $1 \
      \       WHERE (aggregate_uuid, seq_no) IN \
      \   (  SELECT aggregate_uuid, seq_no \
      \        FROM event \
      \       WHERE archive_uuid IS NULL \
      \    ORDER BY aggregate_uuid, seq_no \
      \       LIMIT $2)"

readLatestArchiveMetadata :: Pool Connection -> IO (Maybe ArchiveMetadata)
readLatestArchiveMetadata cp =
    withResource cp $ \c -> withTransaction c $ do
      ioQuery c sqlReadLatestArchiveMetadata [ ] $ SC.map unpackArchiveMetadata >=> Streams.read
    where
      sqlReadLatestArchiveMetadata =
         "SELECT archive_uuid, prev_archive_uuid, next_archive_uuid \
         \  FROM archive \
         \ WHERE next_archive_uuid IS NULL"

readArchiveMetadata :: Pool Connection -> UUID -> IO (Maybe ArchiveMetadata)
readArchiveMetadata cp archiveId =
    withResource cp $ \c -> withTransaction c $ do
      ioQuery c sqlReadArchiveMetadata [ SqlUUID $ Just archiveId ] $ SC.map unpackArchiveMetadata >=> Streams.read
    where
      sqlReadArchiveMetadata =
          "SELECT archive_uuid, prev_archive_uuid, next_archive_uuid \
          \  FROM archive \
          \ WHERE archive_uuid = $1"

readArchive :: Pool Connection -> ArchiveRef -> (InputStream (UUID, PersistedEvent ByteString) -> IO a) -> IO a
readArchive cp archiveRef p =
  withResource cp $ \c -> withTransaction c $ do
    query c archiveRef $ SC.map unpack >=> p
  where
    -- We use two different queries rather than using "IS NOT DISTINCT
    -- FROM" because the query optimization engine in some (all?)
    -- PostgreSQL versions has a lot of trouble optimizing this and
    -- will do table scans.
    query c CurrentArchive           = ioQuery c sqlReadArchiveCurrent [ ]
    query c (NamedArchive archiveId) = ioQuery c sqlReadArchiveNamed [ SqlUUID $ Just archiveId ]
    -- Unpack result columns
    unpack [ SqlUUID (Just aggregateId)
           , SqlUUID (Just eventId)
           , SqlByteArray (Just eventData)
           , SqlInt32 (Just sequenceNumber)
           ] = (aggregateId, PersistedEvent eventData (fromIntegral sequenceNumber) eventId)
    unpack columns = error $ badQueryResultMsg [show archiveRef] columns
    -- SQL
    sqlReadArchiveCurrent =
        "  SELECT aggregate_uuid, event_uuid, event_data, seq_no \
        \    FROM event \
        \   WHERE archive_uuid IS NULL \
        \ORDER BY aggregate_uuid, seq_no ASC"
    sqlReadArchiveNamed =
        "  SELECT aggregate_uuid, event_uuid, event_data, seq_no \
        \   FROM event \
        \  WHERE archive_uuid = $1 \
        \ORDER BY aggregate_uuid, seq_no ASC"

-- | Create an archive store backed by a PostgreSQL connection pool.
newArchiveStore :: IO UUID -> Pool Connection -> IO (ArchiveStore ByteString)
newArchiveStore uuidSupply connectionPool = do
  return $ ArchiveStore
             { asGetUnarchivedEventCount = getUnarchivedEventCount connectionPool
             , asArchiveEvents = archiveEvents connectionPool uuidSupply
             , asReadLatestArchiveMetadata = readLatestArchiveMetadata connectionPool
             , asReadArchiveMetadata = readArchiveMetadata connectionPool
             , asReadArchive = readArchive connectionPool
             }

