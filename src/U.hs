--------------------------------------------------------------------
-- | Description : Machine word sized unsigned integer type
--------------------------------------------------------------------
{-# language MagicHash , UnboxedTuples #-}
module U where
import qualified "prim" U as P
import B
import Stock.Ord
import qualified Z
import {-# SOURCE #-} I

type U# = P.U
data {-# CTYPE "HsWord" #-} U = U U#

pattern Max, Min ∷ U
pattern Max = U P.Max
pattern Min = U P.Min

infixl 7 ×, /, //, %, %%
infixl 6 +, -
(+), (-), (×) ∷ U → U → U
U x + U y = U (x P.+ y)
U x - U y = U (x P.- y)
-- | Low word of signed integer multiply
U x × U y = U (x P.× y)
U x `add` U y = U (x `P.add` y)
U x `sub` U y = U (x `P.sub` y)
U x `mul` U y = U (x `P.mul` y)

-- | Rounds towards 0. The behavior is undefined if the first argument is zero.
quot, rem ∷ U {- ^ divisor -}  → U {- ^ dividend -} → U
(%%), (//) ∷ U {- ^ dividend -}  → U {- ^ divisor -} → U
quot (U y) (U x) = U (P.quot y x)
U x // U y = U (x P.// y)
-- |Satisfies @(add (rem y x) (mul y (quot y x)) == x@. The
--     behavior is undefined if the first argument is zero.
rem (U y) (U x) = U (P.rem y x)
U x %% U y = U (x P.%% y)
-- | Rounds towards 0. The behavior is undefined if the first argument is zero.
quotRem, divMod ∷ U → U → ( U, U )
quotRem (U y) (U x) = case P.quotRem y x of (# q , r #) → (U q , U r)

U x / U y = U (x P./ y)
U x % U y = U (x P.% y)
divMod (U y) (U x) = case P.divMod y x of (# a , b #) → (U a , U b)

addC, subC ∷ U → U → ( U , B )
-- |Add signed integers reporting overflow.
--           First member of result is the sum truncated to an @I@;
--           second member is zero if the true sum fits in an @I@,
--           nonzero if overflow occurred (the sum is either too large
--           or too small to fit in an @I@).
addC (U y) (U x) = case P.addC x y of (# i , b #) → ( U i , B# b)
-- |Subtract signed integers reporting overflow.
--           First member of result is the difference truncated to an @I@;
--           second member is zero if the true difference fits in an @I@,
--           nonzero if overflow occurred (the difference is either too large
--           or too small to fit in an @I@).
subC (U y) (U x) = case P.subC x y of (# i , b #) → ( U i , B# b)

(>),(≥),(<),(≤),(≡),(≠),
  gt,ge,lt,le,eq,ne ∷ U → U → B
U a > U b = B# (a P.> b)
U a ≥ U b = B# (a P.≥ b)
U a < U b = B# (a P.< b)
U a ≤ U b = B# (a P.≤ b) 
U a ≡ U b = B# (a P.≡ b)
U a ≠ U b = B# (a P.≠ b)
U a `gt` U b = B# (P.gt a b)
U a `ge` U b = B# (P.ge a b)
U a `lt` U b = B# (P.lt a b)
U a `le` U b = B# (P.le a b) 
U a `eq` U b = B# (a P.≡ b)
U a `ne` U b = B# (a P.≠ b)

-- * Conversions

toI ∷ U → I
toI (U u) = I (P.toI u)
fromI ∷ I → U
fromI (I i) = U (P.fromI i)

--toF32 ∷ I → F32
--toF32 = int2Float#
--toF64 ∷ I → F64
--toF64 = int2Double#

--toI8# ∷ I → I8#
--toI8# = narrow8Int#
--toI16# ∷ I → I16#
--toI16# = narrow16Int#
--toI32# ∷ I → I32#
--toI32# = narrow32Int#
--toI8 ∷ I → I8
--toI8 = narrowInt8#
--toI16 ∷ I → I16
--toI16 = narrowInt16#


-- | Bitwise negation.
not ∷ U → U
not (U x) = U (P.not x)

-- | Shift left.  Result undefined if shift amount is not
--           in the range 0 to word size - 1 inclusive.
shiftL# ∷ I → U → U
shiftL# (I i) (U x) = U (P.shiftL# i x)


-- | Shift right logical.  Result undefined if shift amount is not
--           in the range 0 to word size - 1 inclusive.

shiftRL# ∷ I → U → U
shiftRL# (I i) (U x) = U (P.shiftRL# i x)
and, or, xor ∷ U → U → U
U x `and` U y = U (P.and x y)
U x `or` U y = U (P.or x y)
U x `xor` U y = U (P.xor x y)

---

fromInteger ∷ Z.Z → U
fromInteger z = U (Z.toU z)
