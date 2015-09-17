module Data.CQRS.Test.Internal.Scope
    ( ScopeM
    , ask
    , mkRunScope
    , randomUUID
    , verify
    ) where

import           Control.Exception (bracket)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import           Data.CQRS.Test.Internal.TestKitSettings
import           Data.UUID.Types (UUID)
import           System.Random (randomIO)

-- Monad providing ambient access to the current spec scope. This is
-- test code, so we don't bother making it opaque.
type ScopeM s r = ReaderT s IO r

-- Verify a property in the current spec scope. This is just an alias
-- for 'lifIO' which can be used to aid readability.
verify :: IO r -> ScopeM s r
verify = liftIO

-- Lifted version of randomUUID. This is just an alias for convenience.
randomUUID :: ScopeM s UUID
randomUUID = liftIO $ randomIO

-- Make a "scope runner" function from a given IO action and test
-- settings. Each scope will be automatically bracketed by the "setUp"
-- and "tearDown" functions of of the test settings.
mkRunScope :: TestKitSettings a ctx -> (a -> IO s) -> (ScopeM s r -> IO r)
mkRunScope testKitSettings mkScope  = \action -> do
  bracket (tksSetUp testKitSettings)
          (tksTearDown testKitSettings) $ \a -> do
    mkScope a >>= runReaderT action
