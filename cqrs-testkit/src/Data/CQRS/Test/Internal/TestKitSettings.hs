module Data.CQRS.Test.Internal.TestKitSettings
    ( TestKitSettings(..)
    , mkBracket
    ) where

import Control.Exception.Base (bracket)

-- | Test kit settings with a test context of type 'ctx'.
data TestKitSettings a ctx = TestKitSettings
    { tksSetUp :: IO a
    , tksTearDown :: a -> IO ()
    , tksMakeContext :: a -> IO ctx
    }

-- | Create a "bracket" for a computation which ensures that
-- the setUp and tearDown phases happen no matter what else
-- happens during the computation.
mkBracket :: TestKitSettings a ctx -> (a -> IO b) -> IO b
mkBracket testKitSettings =
    bracket (tksSetUp testKitSettings)
            (tksTearDown testKitSettings)
