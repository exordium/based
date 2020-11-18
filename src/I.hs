{-# OPTIONS_GHC -Wno-deprecations #-}
{-# language MagicHash , UnboxedTuples #-}
module I where
import qualified "prim" I as P
import B
import Stock.Ord
import qualified Z
import {-# SOURCE #-} U

type I# = P.I
-- | A fixed-precision integer type with at least the range @[-2^29 .. 2^29-1]@.
-- The exact range for a given implementation can be determined by using
-- 'Prelude.minBound' and 'Prelude.maxBound' from the 'Prelude.Bounded'
-- class.
data {-# CTYPE "HsInt" #-} I = I I#

pattern Max, Min ∷ I
pattern Max = I P.Max
pattern Min = I P.Min

infixl 7 *, ⁄, ⁄⁄, %, %%
infixl 6 +, -
(+), (-), (*) ∷ I → I → I
I x + I y = I (x P.+ y)
I x - I y = I (x P.- y)
-- | Low word of signed integer multiply
I x * I y = I (x P.* y)
I x `add` I y = I (x `P.add` y)
I x `sub` I y = I (x `P.sub` y)
I x `mul` I y = I (x `P.mul` y)

-- |Return non-zero if there is any possibility that the upper word of a
--     signed integer multiply might contain useful information.  Return
--     zero only if you are completely sure that no overflow can occur.
--     On a 32-bit platform, the recommended implementation is to do a
--     32 x 32 → 64 signed multiply, and subtract result[63:32] from
--     (result[31] >>signed 31).  If this is zero, meaning that the
--     upper word is merely a sign extension of the lower one, no
--     overflow can occur.
--
--     On a 64-bit platform it is not always possible to
--     acquire the top 64 bits of the result.  Therefore, a recommended
--     implementation is to take the absolute value of both operands, and
--     return 0 iff bits[63:31] of them are zero, since that means that their
--     magnitudes fit within 31 bits, so the magnitude of the product must fit
--     into 62 bits.
--
--     If in doubt, return non-zero, but do make an effort to create the
--     correct answer for small args, since otherwise the performance of
--     @(*) ∷ I → I → I@ will be poor.
mulMayOflo ∷ I → I → B
mulMayOflo (I x) (I y) = B# (P.mulMayOflo x y)
negate ∷ I → I
negate (I x) = I (P.negate x)
-- | Rounds towards 0. The behavior is undefined if the first argument is zero.
quot, rem ∷ I {- ^ divisor -}  → I {- ^ dividend -} → I
(%%), (⁄⁄) ∷ I {- ^ dividend -}  → I {- ^ divisor -} → I
quot (I y) (I x) = I (P.quot y x)
I x ⁄⁄ I y = I (x P.⁄⁄ y)
-- |Satisfies @(add (rem y x) (mul y (quot y x)) == x@. The
--     behavior is undefined if the first argument is zero.
rem (I y) (I x) = I (P.rem y x)
I x %% I y = I (x P.%% y)
-- | Rounds towards 0. The behavior is undefined if the first argument is zero.
quotRem ∷ I → I → ( I, I )
quotRem (I y) (I x) = case P.quotRem y x of (# q , r #) → (I q , I r)

-- These functions have built-in rules.
-- | Rounds towards -∞. The behavior is undefined if the first argument is zero.
div,mod ∷ I {- ^ divisor -} → I {- ^ dividend -} → I
(%), (⁄) ∷ I {- ^ dividend -}  → I {- ^ divisor -} → I
div (I y) (I x) = I (P.div y x)
mod (I y) (I x) = I (P.mod y x)
I x ⁄ I y = I (x P.⁄ y)
I x % I y = I (x P.% y)
-- | Rounds towards -∞. The behavior is undefined if the first argument is zero.
divMod (I y) (I x) = case P.divMod y x of (# a , b #) → (I a , I b)

addC, subC ∷ I → I → ( I , B )
-- |Add signed integers reporting overflow.
--           First member of result is the sum truncated to an @I@;
--           second member is zero if the true sum fits in an @I@,
--           nonzero if overflow occurred (the sum is either too large
--           or too small to fit in an @I@).
addC (I y) (I x) = case P.addC x y of (# i , b #) → ( I i , B# b)
-- |Subtract signed integers reporting overflow.
--           First member of result is the difference truncated to an @I@;
--           second member is zero if the true difference fits in an @I@,
--           nonzero if overflow occurred (the difference is either too large
--           or too small to fit in an @I@).
subC (I y) (I x) = case P.subC x y of (# i , b #) → ( I i , B# b)

(>),(≥),(<),(≤),(≡),(≠),
  gt,ge,lt,le,eq,ne ∷ I → I → B
I a > I b = B# (a P.> b)
I a ≥ I b = B# (a P.≥ b)
I a < I b = B# (a P.< b)
I a ≤ I b = B# (a P.≤ b) 
I a ≡ I b = B# (a P.≡ b)
I a ≠ I b = B# (a P.≠ b)
I a `gt` I b = B# (P.gt a b)
I a `ge` I b = B# (P.ge a b)
I a `lt` I b = B# (P.lt a b)
I a `le` I b = B# (P.le a b) 
I a `eq` I b = B# (a P.≡ b)
I a `ne` I b = B# (a P.≠ b)

-- * Conversions

toU ∷ I → U
toU (I i) = U (P.toU i)
fromU ∷ U → I
fromU (U u) = I (P.fromU u)

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


-- |Shift right arithmetic.  Result undefined if shift amount is not
--           in the range 0 to word size - 1 inclusive.
shiftR# ∷ I → I → I
shiftR# (I i) (I j) = I (P.shiftR# i j)

-- | Bitwise negation. @not n = -n - 1@
not ∷ I → I
not (I x) = I (P.not x)

{-# DEPRECATED shiftL#, shiftRL#, and, or, xor "Signed logical bitwise operations are rarely sensible, prefer U instead" #-}

-- | Shift left.  Result undefined if shift amount is not
--           in the range 0 to word size - 1 inclusive.
shiftL# ∷ I → I → I
shiftL# (I i) (I x) = I (P.shiftL# i x)


-- | Shift right logical.  Result undefined if shift amount is not
--           in the range 0 to word size - 1 inclusive.

shiftRL# ∷ I → I → I
shiftRL# (I i) (I x) = I (P.shiftRL# i x)
and, or, xor ∷ I → I → I
I x `and` I y = I (P.and x y)
I x `or` I y = I (P.or x y)
I x `xor` I y = I (P.xor x y)

---

abs ∷ I → I
abs n@(I n#) = if B# (n# P.≥ 0#) then n else negate n

signum n@(I n#) | B# (n# P.< 0#) = I (-1#)
                | B# (n# P.≡ 0#) = I 0#
fromInteger ∷ Z.Z → I
fromInteger z = I (Z.toI z)
