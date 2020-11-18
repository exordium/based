{-# OPTIONS_GHC -Wno-deprecations #-}
{-# language MagicHash , UnboxedTuples #-}
module I.I8 (module I.I8) where
import qualified "prim" I.I8 as P
import "prim" I.I8 as X (I8#)
import qualified "prim" I
import B
import Stock.Ord
import qualified Z
import {-# source #-} I as X (I(..))
import qualified GHC.Int as GHC (Int8(..))

-- | A fixed-precision integer type with at least the range @[-2^29 .. 2^29-1]@.
-- The exact range for a given implementation can be determined by using
-- 'Prelude.minBound' and 'Prelude.maxBound' from the 'Prelude.Bounded'
-- class.
-- data {-# CTYPE "HsInt8" #-} I8 = I8# P.I8#
type I8 = GHC.Int8
pattern I8# ∷ I8# → I8
pattern I8# i = GHC.I8# i
pattern Max, Min ∷ I8
pattern Max = I8# P.Max
pattern Min = I8# P.Min

infixl 7 *, ⁄, ⁄⁄, %, %%
infixl 6 +, -
(+), (-), (*) ∷ I8 → I8 → I8
I8# x + I8# y = I8# (P.fromI (x I.+ y))
I8# x - I8# y = I8# (P.fromI (x I.- y))
-- | Low word of signed integer multiply
I8# x * I8# y = I8# (P.fromI (x I.* y))

_divZeroError = let x = x in x
_overflow = let x = x in x
_overflowError = let x = x in x

negate ∷ I8 → I8
negate (I8# x) = I8# (P.fromI (I.negate x))
-- | Rounds towards 0. The behavior is undefined if the first argument is zero.
quot, rem ∷ I8 {- ^ divisor -}  → I8 {- ^ dividend -} → I8
(%%), (⁄⁄) ∷ I8 {- ^ dividend -}  → I8 {- ^ divisor -} → I8
quot (I8# y) (I8# x) | B# (y I.≡ 0#) = _divZeroError
                     | B# (y I.≡ (-1#) ∧# x I.≡ P.Min) = _overflow 
                     | T = I8# (P.fromI (I.quot y x))
I8# x ⁄⁄ I8# y | B# (y I.≡ 0#) = _divZeroError
               | B# (y I.≡ (-1#) ∧# x I.≡ P.Min) = _overflow 
               | T = I8# do P.fromI (x I.⁄⁄ y)
-- |Satisfies @x %% y + (x ⁄⁄ y) * y == x@. The
--     behavior is undefined if the first argument is zero.
rem (I8# y) (I8# x) | B# (y I.≡ 0#) = _divZeroError
                    | T = I8# (P.fromI (I.rem y x))
I8# x %% I8# y | B# (y I.≡ 0#) = _divZeroError
               | T = I8# do P.fromI (x I.%% y)
-- | Rounds towards 0. The behavior is undefined if the first argument is zero.
quotRem ∷ I8 → I8 → ( I8, I8 )
quotRem (I8# y) (I8# x) | B# (y I.≡ 0#) = _divZeroError
                        | B# (y I.≡ (-1#) ∧# x I.≡ P.Min) = (_overflowError , I8# 0#)
                        | T = case I.quotRem y x of
                          (# q , r #) → (I8# do P.fromI q , I8# do P.fromI r)

-- | Rounds towards -∞. The behavior is undefined if the first argument is zero.
div,mod ∷ I8 {- ^ divisor -} → I8 {- ^ dividend -} → I8
(%), (⁄) ∷ I8 {- ^ dividend -}  → I8 {- ^ divisor -} → I8
div (I8# y) (I8# x) | B# (y I.≡ 0#) = _divZeroError
                    | B# (y I.≡ (-1#) ∧# x I.≡ P.Min) = _overflow 
                    | T = I8# (P.fromI (I.div y x))
I8# x ⁄ I8# y | B# (y I.≡ 0#) = _divZeroError
              | B# (y I.≡ (-1#) ∧# x I.≡ P.Min) = _overflow 
              | T = I8# do P.fromI (x I.⁄ y)
mod (I8# y) (I8# x) | B# (y I.≡ 0#) = _divZeroError
                    | T = I8# (P.fromI (I.mod y x))
I8# x % I8# y | B# (y I.≡ 0#) = _divZeroError
              | T = I8# do P.fromI (x I.% y)
-- | Rounds towards -∞. The behavior is undefined if the first argument is zero.
divMod (I8# y) (I8# x) | B# (y I.≡ 0#) = _divZeroError
                      | B# (y I.≡ (-1#) ∧# x I.≡ P.Min) = (_overflowError , I8# 0#)
                      | T = case I.divMod y x of
                         (# q , r #) → (I8# do P.fromI q , I8# do P.fromI r)

(>),(≥),(<),(≤),(≡),(≠),
  gt,ge,lt,le,eq,ne ∷ I8 → I8 → B
I8# a > I8# b = B# (a I.> b)
I8# a ≥ I8# b = B# (a I.≥ b)
I8# a < I8# b = B# (a I.< b)
I8# a ≤ I8# b = B# (a I.≤ b) 
I8# a ≡ I8# b = B# (a I.≡ b)
I8# a ≠ I8# b = B# (a I.≠ b)
I8# a `gt` I8# b = B# (I.gt a b)
I8# a `ge` I8# b = B# (I.ge a b)
I8# a `lt` I8# b = B# (I.lt a b)
I8# a `le` I8# b = B# (I.le a b) 
I8# a `eq` I8# b = B# (a I.≡ b)
I8# a `ne` I8# b = B# (a I.≠ b)

-- * Conversions

fromI ∷ I → I8
fromI (I i) = I8# do P.fromI i

--toU ∷ I8 → U8
--toU (I8 i) = U (P.toU i)
--fromU ∷ U8 → I8
--fromU (U u) = I8 (P.fromU u)

--toF32 ∷ I8 → F32
--toF32 = int2Float#
--toF64 ∷ I8 → F64
--toF64 = int2Double#

--toI88# ∷ I8 → I88#
--toI88# = narrow8I8nt#
--toI816# ∷ I8 → I816#
--toI816# = narrow16I8nt#
--toI832# ∷ I8 → I832#
--toI832# = narrow32I8nt#
--toI88 ∷ I8 → I88
--toI88 = narrowI8nt8#
--toI816 ∷ I8 → I816
--toI816 = narrowI8nt16#


-- |Shift right arithmetic.  Result undefined if shift amount is not
--           in the range 0 to word size - 1 inclusive.
shiftR# ∷ I8 → I8 → I8
shiftR# (I8# i) (I8# x) = I8# (I.shiftR i x)

-- | Bitwise negation. @not n = -n - 1@
not ∷ I8 → I8
not (I8# x) = I8# (I.not x)

{-# DEPRECATED shiftL#, shiftRL#, and, or, xor "Signed logical bitwise operations are rarely sensible, prefer U instead" #-}

-- | Shift left.  Result 0 if shift amount is not
--           in the range 0 to 7 inclusive.
shiftL# ∷ I8 → I8 → I8
shiftL# (I8# i) (I8# x) = I8# (P.fromI (I.shiftL i x))


-- | Shift right logical.  Result undefined if shift amount is not
--           in the range 0 to word size - 1 inclusive.

shiftRL# ∷ I8 → I8 → I8
shiftRL# (I8# i) (I8# x) = I8# (P.fromI (I.shiftRL# i x))
and, or, xor ∷ I8 → I8 → I8
I8# x `and` I8# y = I8# (I.and x y)
I8# x `or` I8# y = I8# (I.or x y)
I8# x `xor` I8# y = I8# (I.xor x y)

---

abs ∷ I8 → I8
abs n@(I8# n#) = if B# (n# I.≥ 0#) then n else negate n

signum n@(I8# n#) | B# (n# I.< 0#) = I8# (-1#)
                  | B# (n# I.≡ 0#) = I8# 0#
fromI8nteger ∷ Z.Z → I8
fromI8nteger z = I8# (P.fromI (Z.toI z))


foreign import ccall "foo" foo ∷ I8
