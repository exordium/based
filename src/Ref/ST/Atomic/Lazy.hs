--------------------------------------------------------------------
-- | Description : 
--
-- 'modify' and 'modify2' do not apply the function strictly.  This is important
-- to know even if all you are doing is replacing the value.  For example, this
-- will leak memory:
--
-- >r ← new '1'
-- >(∞) do modify r \ _ → ('2', ())
--
-- Use "Ref.ST.Atomic" to avoid this problem.
--------------------------------------------------------------------
{-# language MagicHash,UnboxedTuples #-}
module Ref.ST.Atomic.Lazy where
import qualified "prim" Ref.Boxed as Prim
import ST
import "prim" ST (Token)
import Ref.ST (Ref(..))
import qualified Ref.ST as ST
import Result (type (!)(..))
import B

import qualified I
import Debug
import Stock.Char
-- | Atomically apply a function to the contents and return the old and new values.
-- The result of the function is not forced.
-- As this can lead to a memory leak,
-- it is usually better to use 'Ref.ST.Atomic.modify'.
modify ∷ Ref s a → (a → a) → ST s (a , a) -- ^ Old value and modified value
modify (Ref r) f = ST# \ s →
  case Prim.modify r f s of (# s , (# old , new #) #) → (# s , ( old , new) #)

-- | Atomically apply a function to the contents and return the old value,  new value, and return value.
-- The results of the function are not forced.
-- As this can lead to a memory leak,
-- it is usually better to use 'Ref.ST.Atomic.modify2'.
modify2 ∷ Ref s a → (a → (a,b)) → ST s (a , (a,b)) -- ^ Old value and modified/return value
modify2 (Ref r) f = ST# \ s → case Prim.modify2 r f s of
  (# s' , (# a , ab #) #) → (# s' , (a , ab) #)

-- | Compare and atomically swap if the old value matches expected.
cas ∷ Ref s a
    → a -- ^ expected old value
    → a -- ^ new value
    → ST s (a ! a) -- ^ @Err oldValue@ if swap failed, or @Ok currentValue@
cas (Ref r) old new = ST# \ s →
  case Prim.cas r old new s of (# s' , (# b , a #) #) | B# b → (# s , Err a #)
                                                      | T    → (# s , Ok  a #)
