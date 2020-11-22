--------------------------------------------------------------------
-- | Description : Atomic operations on ST Refs
--
-- For using 'Ref.ST.Ref' in a safe way in a multithreaded program.
-- If you only have one 'Ref.ST.Ref', then using these operations to
-- access and modify it will prevent race conditions.
--
-- Extending the atomicity to multiple 'Ref's is problematic, so it
-- is recommended that if you need to do anything more complicated
-- then using "Ref.Block" instead is a good idea.
------------------------------------------------------------------

{-# language MagicHash,UnboxedTuples #-}
module Ref.ST.Atomic where
import qualified Ref.ST.Atomic.Lazy as Lazy
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
-- Forces the result.
modify ∷ Ref s a → (a → a) → ST s a -- ^ Modified value
modify r f = do (_,!b) ← Lazy.modify r f; η b
modify_ ∷ Ref s a → (a → a) → ST s ()
modify_ r f = do _ ← modify r f; η ()

-- | Forces both the value stored in the 'Ref' and the value returned.
-- The new value is installed in before the returned value is forced.
-- So
--
-- @modify2 r \ x → (x+1, (⊥))@
--
-- will increment the 'Ref' and then throw an exception in the calling thread.
modify2 ∷ Ref s a → (a → (a,b)) → ST s b
modify2 r f = do
  (_ , (_ , !b)) ← Lazy.modify2 r \ a → case f a of r@(!_,_) → r
  η b

-- | Forces both the value stored in the 'Ref' and the (old) value returned.
swap ∷ Ref s a -> a -> ST s a {- ^ Old value -}
swap r a = do (!a0,!_) ← Lazy.modify r \ _ → a; η a0

write ∷ Ref s a → a → ST s ()
write r a = do _ ← swap r a; η ()

-- | Compare and swap if the old value matches expected.
-- Forces the new value installed.
cas ∷ Ref s a
    → a -- ^ expected old value
    → a -- ^ new value
    → ST s (a ! a) -- ^ @Err oldValue@ if swap failed, or @Ok currentValue@
cas (Ref r) old !new = ST# \ s →
  case Prim.cas r old new s of (# s' , (# b , a #) #) | B# b → (# s , Err a #)
                                                       | T    → (# s , Ok  a #)
