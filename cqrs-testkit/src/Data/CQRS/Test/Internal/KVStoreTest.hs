{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Data.CQRS.Test.Internal.KVStoreTest
    ( mkKVStoreSpec
    ) where

import           Data.Aeson (Value(..))
import           Data.ByteString (ByteString)
import           Data.CQRS.KVStore
import           Data.CQRS.Test.Internal.TestKitSettings
import           Data.CQRS.Test.Internal.Scope (verify, ask)
import qualified Data.CQRS.Test.Internal.Scope as S
import           Data.CQRS.Test.Internal.Utils (randomId)
import           Test.Hspec (Spec, describe, shouldBe, shouldContain)
import qualified Test.Hspec as Hspec
import qualified UnliftIO.Streams.List as SL

-- Ambient data for test scope for each spec.
newtype Scope i a = Scope { scopeKVStore :: KVStore i a
                          }

-- Test suite for event store which stores 'ByteString' events.
mkKVStoreSpec :: TestKitSettings a (KVStore ByteString Value) -> Spec
mkKVStoreSpec testKitSettings =

  describe "KVStore implementation" $ do

    it "'insert' should insert if value does not exist already" $ \KVStore{..} -> do
      -- Setup
      k <- randomId
      let expectedValue = "hello, world"
      -- Excercise: Write a value
      kvsInsert k expectedValue
      -- Verify
      Just actualValue <- kvsLookup k
      verify $ actualValue `shouldBe` expectedValue

    it "'insert' should overwrite if value exists already" $ \KVStore{..} -> do
      -- Setup
      k <- randomId
      let overwrittenValue = "goodbye"
      let expectedValue = "hello"
      -- Exercise
      kvsInsert k overwrittenValue
      kvsInsert k expectedValue
      -- Verify
      Just actualValue <- kvsLookup k
      verify $ actualValue `shouldBe` expectedValue

    it "'delete' should do nothing if value does not exist" $ \KVStore{..} -> do
      -- Setup
      k <- randomId
      -- Exercise
      kvsDelete k

    it "'delete' should delete value if it exists" $ \KVStore{..} -> do
      -- Setup
      k <- randomId
      kvsInsert k "hello, world" -- We assume this works per other tests
      -- Exercise
      kvsDelete k
      -- Verify
      result <- kvsLookup k
      verify $ result `shouldBe` Nothing

    it "'traverse' should \"see\" all stored key-value pairs" $ \KVStore{..} -> do
      -- Setup
      k1 <- randomId
      k2 <- randomId
      kvsInsert k1 "hello"
      kvsInsert k2 "world"
      -- Exercise
      keyValues <- kvsTraverse SL.toList
      -- Verify
      verify $ keyValues `shouldContain` [(k1, "hello")]
      verify $ keyValues `shouldContain` [(k2, "world")]

  where
    runScope = S.mkRunScope testKitSettings $ \a -> do
                                     eventStore <- tksMakeContext testKitSettings a
                                     return $ Scope eventStore
    -- Shorthands
    it msg scope = Hspec.it msg $ runScope (fmap scopeKVStore ask >>= scope)
