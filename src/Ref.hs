{-# language MagicHash #-}
module Ref where
import Prelude
import qualified "prim" Ref.Byte as Byte
import qualified "prim" I
import I (I(..))
import B

data Ref a = Ref Byte.Ref deriving Eq
type role Ref phantom

-- | hack to expose nullAddr#
pattern Null ∷ Ref a
pattern Null = Ref Byte.Null

-- |Advances the given address by the given offset in bytes.
(∔) ∷ I → Ref a → Ref a
I i ∔ Ref q = Ref do i Byte.∔ q

-- |Computes the offset required to get from the second to the first argument. 
(⨪) ∷ Ref a → Ref a → I
Ref p ⨪ Ref q = I do p Byte.⨪ q

(.//) ∷ Ref a → I → I
Ref p .// I i = I do p Byte..// i

-- |Given an arbitrary address and an alignment constraint,
-- 'align' yields the next higher address that fulfills the
-- alignment constraint.  An alignment constraint @x@ is fulfilled by
-- any address divisible by @x@.  This operation is idempotent.
align :: Ref a -> I -> Ref a
align r@(Ref p) (I i)
  = case p Byte..// i of {
      0# -> r;
      n -> Ref do (i I.- n) Byte.∔ p }

(>), (≥), (<), (≤), (≡), (≠) ∷ Ref a → Ref a → B
Ref p > Ref q = B# (p Byte.> q); Ref p ≥ Ref q = B# (p Byte.≥ q)
Ref p < Ref q = B# (p Byte.< q); Ref p ≤ Ref q = B# (p Byte.≤ q)
Ref p ≡ Ref q = B# (p Byte.≡ q); Ref p ≠ Ref q = B# (p Byte.≠ q)
