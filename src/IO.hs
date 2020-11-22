{-# language MagicHash, UnboxedTuples #-}
module IO (IO, module IO) where
import Stock.IO
import "prim" IO
import "prim" Thread
import GHC.Show
import ST (ST(..))

pattern IO# ∷ IO# a → IO a
pattern IO# io = IO io
{-# complete IO# #-}

--run# ∷ (∀ s. ST s a) → a; [># inline run #<]
--run# (ST# st) = case IO.run st of (# _ , a #) → a

φ ∷ (a → b) → IO a → IO b
φ f (IO# m) = IO# \ s -> case m s of
  (# s', r #) -> (# s', f r #)

η ∷ x → IO x; {-# inline η #-}
η x = IO# \ s → (# s , x #)

μ ∷ IO (IO x) → IO x; {-# inline μ #-}
μ (IO# mm) = IO# \ s → case mm s of (# s' , IO# m #) → m s'

IO# m >> IO# n = IO# \ s → case m s of (# s' , _ #) → n s'
IO# m >>= f = IO# \ s → case m s of (# s' , a #) → case f a of IO# n → n s'

-- | Allows an 'IO' computation to be deferred lazily until the value is demanded. Eg for lazy file reading
interleave# ∷ IO a → IO a
interleave# m = dupableInterleave# (do _ ← noDuplicate#; m)

-- | Allows an 'IO' computation to be deferred lazily until the value is demanded
--
-- The computation may be performed multiple times by different threads,
-- possibly at the same time. To prevent this, use 'interleave' instead.
{-# NOINLINE dupableInterleave# #-}
-- See Note [unsafeDupableInterleaveIOhould not be inlined]
-- in GHC.IO.Unsafe
dupableInterleave# :: IO a -> IO a
dupableInterleave# (IO# m) = IO# \ s ->
    let r = case m s of (# _, res #) -> res
    in (# s, r #)

noDuplicate# ∷ IO ()
noDuplicate# = IO# \ s → case Thread.noDuplicate s of s' → (# s' , () #)

fromST ∷ ST (☸) a → IO a
fromST (ST# k) = IO# k
toST ∷ IO a → ST (☸) a
toST (IO# a) = ST# a
