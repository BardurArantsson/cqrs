module Data.CQRS.Types.Iso
    ( Iso
    ) where

-- | Isomorphism between 'a' and 'b.
type Iso a b = (a -> b, b -> a)
