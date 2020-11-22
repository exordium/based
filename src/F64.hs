--------------------------------------------------------------------
-- | Description : 64-bit floating point type
--------------------------------------------------------------------
{-# language MagicHash , UnboxedTuples #-}
module F64 where
import qualified "prim" F64 as P
import B
import Stock.Ord
import qualified Z
import {-# source #-} I

type F64# = P.F64
-- | A fixed-precision integer type with at least the range @[-2^29 .. 2^29-1]@.
-- The exact range for a given implementation can be determined by using
-- 'Prelude.minBound' and 'Prelude.maxBound' from the 'Prelude.Bounded'
-- class.
data {-# CTYPE "HsWord" #-} F64 = F64 F64#

infixl 7 ×
infixl 6 +, -
(+), (-), (×) ∷ F64 → F64 → F64
F64 x + F64 y = F64 (x P.+ y)
F64 x - F64 y = F64 (x P.- y)
-- | Low word of signed integer multiply
F64 x × F64 y = F64 (x P.× y)
F64 x `add` F64 y = F64 (x `P.add` y)
F64 x `sub` F64 y = F64 (x `P.sub` y)
F64 x `mul` F64 y = F64 (x `P.mul` y)

(>),(≥),(<),(≤),(≡),(≠),
  gt,ge,lt,le,eq,ne ∷ F64 → F64 → B
F64 a > F64 b = B# (a P.> b)
F64 a ≥ F64 b = B# (a P.≥ b)
F64 a < F64 b = B# (a P.< b)
F64 a ≤ F64 b = B# (a P.≤ b) 
F64 a ≡ F64 b = B# (a P.≡ b)
F64 a ≠ F64 b = B# (a P.≠ b)
F64 a `gt` F64 b = B# (P.gt a b)
F64 a `ge` F64 b = B# (P.ge a b)
F64 a `lt` F64 b = B# (P.lt a b)
F64 a `le` F64 b = B# (P.le a b) 
F64 a `eq` F64 b = B# (a P.≡ b)
F64 a `ne` F64 b = B# (a P.≠ b)

pow ∷ F64 → F64 → F64
pow (F64 x) (F64 y) = F64 (P.pow x y)

negate,abs,exp,log,sqrt,sin,cos,tan,asin,acos,atan,sinh,cosh,tanh ∷ F64 → F64
negate (F64 x) = F64 (P.negate x)
abs (F64 x) = F64 (P.abs x)
exp (F64 x) = F64 (P.exp x); log (F64 x) = F64 (P.log x)
sqrt (F64 x) = F64 (P.sqrt x)
sin (F64 x) = F64 (P.sin x)
cos (F64 x) = F64 (P.cos x)
tan (F64 x) = F64 (P.tan x)
asin (F64 x) = F64 (P.asin x)
acos (F64 x) = F64 (P.acos x)
atan (F64 x) = F64 (P.atan x)
sinh (F64 x) = F64 (P.sinh x)
cosh (F64 x) = F64 (P.cosh x)
tanh (F64 x) = F64 (P.tanh x)


-- * Conversions

toI ∷ F64 → I
toI (F64 u) = I (P.toI u)
fromI ∷ I → F64
fromI (I i) = F64 (P.fromI i)

--toF32 ∷ F64 → F64
--toF32 = int2Double#
--fromF32 ∷ F64 → F64

--decode2I ∷ F64 → (# I8#, U32#, U32#, I16# #) -- ^ (sign {1,-1}, high, low, exp)
--decodeI64 ∷ F64 → (# I64, I16# #) -- ^ (mantissa , base-2 exponent)
