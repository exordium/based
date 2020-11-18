{-# language MagicHash, UnboxedTuples #-}
module ST where
import "prim" ST as P
import qualified "prim" IO
import "prim" Thread
import GHC.Show
import qualified Syntax
import Stock.Int

newtype ST s a = ST# (ST# s a)

run ∷ (∀ s. ST s a) → a; {-# inline run #-}
run (ST# st) = case IO.run st of (# _ , a #) → a

φ ∷ (a → b) → ST s a → ST s b
φ f (ST# m) = ST# \ s -> case m s of
  (# s', r #) -> (# s', f r #)
instance Syntax.Functor (ST s) where
  fmap = φ
  a <$ ST# st = ST# \ s → case st s of (# s' , _ #) → (# s' , a #)

η ∷ x → ST s x; {-# inline η #-}
η x = ST# \ s → (# s , x #)
instance Syntax.Applicative (ST s) where
  pure = η
  stf <*> sta = do f ← stf ; a ← sta ; η (f a)
  m *> n = μ (n Syntax.<$ m)

μ ∷ ST s (ST s a) → ST s a; {-# inline μ #-}
μ (ST# mm) = ST# \ s → case mm s of (# s' , ST# m #) → m s'
instance Syntax.Monad (ST s) where
  m >>= f = μ (φ f m)
  (>>) = (*>)

ST# m *> ST# n = ST# \ s → case m s of (# s' , _ #) → n s'
(>>) = (*>)
ST# m >>= f = ST# \ s → case m s of (# s' , a #) → case f a of ST# n → n s'

-- | Allows an 'ST' computation to be deferred lazily until the value is demanded
interleave# ∷ ST s a → ST s a
interleave# m = interleave## (do noDuplicate#; m)

-- | Allows an 'ST' computation to be deferred lazily until the value is demanded
--
-- The computation may be performed multiple times by different threads,
-- possibly at the same time. To prevent this, use 'interleave' instead.
{-# NOINLINE interleave## #-}
interleave## :: ST s a -> ST s a
interleave## (ST# m) = ST# \ s ->
    let r = case m s of (# _, res #) -> res
    in (# s, r #)

noDuplicate# ∷ ST s ()
noDuplicate# = ST# \ s → case Thread.noDuplicate s of s' → (# s' , () #)

instance Show (ST s a) where
  showsPrec _ _ = showString "<<ST>>"
  showList = showList__ (showsPrec (I# 0#))

