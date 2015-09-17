{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
module Data.CQRS.Test.Internal.SnapshotTest
    ( mkSnapshotStoreSpec
    ) where

import           Control.Monad.IO.Class (liftIO)
import           Data.ByteString (ByteString)
import           Data.CQRS.Test.Internal.Scope (ScopeM, verify, ask, randomUUID)
import qualified Data.CQRS.Test.Internal.Scope as S
import           Data.CQRS.Test.Internal.TestKitSettings
import           Data.CQRS.Types.Snapshot
import           Data.CQRS.Types.SnapshotStore
import           Data.UUID.Types (UUID)
import           Test.Hspec (Spec, describe, shouldBe)
import qualified Test.Hspec as Hspec

-- Write snapshot
writeSnapshot :: UUID -> Snapshot a -> ScopeM (SnapshotStore a) ()
writeSnapshot aggregateId snapshot = do
  snapshotStore <- ask
  liftIO $ ssWriteSnapshot snapshotStore aggregateId snapshot

-- Read a snapshot
readSnapshot :: UUID -> ScopeM (SnapshotStore a) (Maybe (Snapshot a))
readSnapshot aggregateId = do
  snapshotStore <- ask
  liftIO $ ssReadSnapshot snapshotStore aggregateId

-- Test suite for memory backend.
mkSnapshotStoreSpec :: TestKitSettings a (SnapshotStore ByteString) -> Spec
mkSnapshotStoreSpec testKitSettings = do

  describe "SnapshotStore" $ do

    it "writing first snapshot works" $ do
      aggregateId <- randomUUID
      -- Write the snapshot.
      let rs = (Snapshot 3 "Hello, world")
      writeSnapshot aggregateId rs
      -- Read the snapshot.
      rs' <- readSnapshot aggregateId
      -- Assert that we've retrieved the right snapshot.
      verify $ rs' `shouldBe` Just rs

    it "updating snapshot overwrites existing one" $ do
      aggregateId <- randomUUID
      -- Write snapshot "twice"
      let rs1 = (Snapshot 3 "Hello")
      let rs2 = (Snapshot 4 "Goodbye")
      writeSnapshot aggregateId rs1
      writeSnapshot aggregateId rs2
      -- Read latest snapshot
      rs' <- readSnapshot aggregateId
      -- Assert that the latter snapshot was retrieved
      verify $ rs' `shouldBe` Just rs2

  where
    runScope = S.mkRunScope testKitSettings (tksMakeContext testKitSettings)
    it msg = Hspec.it msg . runScope
