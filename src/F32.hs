{-# language MagicHash , UnboxedTuples #-}
module F32 where
import qualified "prim" F32 as P
import B
import Stock.Ord
import qualified Z
import {-# source #-} I

type F32# = P.F32
-- | A fixed-precision integer type with at least the range @[-2^29 .. 2^29-1]@.
-- The exact range for a given implementation can be determined by using
-- 'Prelude.minBound' and 'Prelude.maxBound' from the 'Prelude.Bounded'
-- class.
data {-# CTYPE "HsWord" #-} F32 = F32 F32#

infixl 7 ×
infixl 6 +, -
(+), (-), (×) ∷ F32 → F32 → F32
F32 x + F32 y = F32 (x P.+ y)
F32 x - F32 y = F32 (x P.- y)
-- | Low word of signed integer multiply
F32 x × F32 y = F32 (x P.× y)
F32 x `add` F32 y = F32 (x `P.add` y)
F32 x `sub` F32 y = F32 (x `P.sub` y)
F32 x `mul` F32 y = F32 (x `P.mul` y)

(>),(≥),(<),(≤),(≡),(≠),
  gt,ge,lt,le,eq,ne ∷ F32 → F32 → B
F32 a > F32 b = B# (a P.> b)
F32 a ≥ F32 b = B# (a P.≥ b)
F32 a < F32 b = B# (a P.< b)
F32 a ≤ F32 b = B# (a P.≤ b) 
F32 a ≡ F32 b = B# (a P.≡ b)
F32 a ≠ F32 b = B# (a P.≠ b)
F32 a `gt` F32 b = B# (P.gt a b)
F32 a `ge` F32 b = B# (P.ge a b)
F32 a `lt` F32 b = B# (P.lt a b)
F32 a `le` F32 b = B# (P.le a b) 
F32 a `eq` F32 b = B# (a P.≡ b)
F32 a `ne` F32 b = B# (a P.≠ b)

negate,abs,exp,log,sqrt,sin,cos,tan,asin,acos,atan,sinh,cosh,tanh ∷ F32 → F32
negate (F32 x) = F32 (P.negate x)
abs (F32 x) = F32 (P.abs x)
exp (F32 x) = F32 (P.exp x); log (F32 x) = F32 (P.log x)
sqrt (F32 x) = F32 (P.sqrt x)
sin (F32 x) = F32 (P.sin x)
cos (F32 x) = F32 (P.cos x)
tan (F32 x) = F32 (P.tan x)
asin (F32 x) = F32 (P.asin x)
acos (F32 x) = F32 (P.acos x)
atan (F32 x) = F32 (P.atan x)
sinh (F32 x) = F32 (P.sinh x)
cosh (F32 x) = F32 (P.cosh x)
tanh (F32 x) = F32 (P.tanh x)

-- * Conversions

toI ∷ F32 → I
toI (F32 u) = I (P.toI u)
fromI ∷ I → F32
fromI (I i) = F32 (P.fromI i)

--toF64 ∷ F32 → F64
--toF64 = int2Double#
--fromF64 ∷ F64 → F32

-- decode ∷ F32 → (# I32# , I32# #)
