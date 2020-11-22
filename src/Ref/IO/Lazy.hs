{-# language MagicHash,UnboxedTuples #-}
module Ref.IO.Lazy where
import qualified "prim" Ref.Boxed as Prim
import IO
import B
import Stock.Eq
import {-# source #-} Ref.IO
import qualified Ref.ST as ST


pattern Ref ∷ Prim.Ref (☸) a → Ref a
pattern Ref p = Ref# (ST.Ref p)

(≡) ∷ Ref a → Ref a → B
Ref p ≡ Ref q = B# (p Prim.≡ q)
instance Eq (Ref a) where (==) = (≡)

new ∷ a → IO (Ref a)
new a = IO# \ s → case Prim.new a s of (# s', r #) → (# s', Ref r #)

read ∷ Ref a → IO a
read (Ref r) = IO# do Prim.read r

write ∷ Ref a → a → IO ()
write (Ref r) a = IO# \ s → case Prim.write r a s of s' → (# s', () #)

modify ∷ Ref a → (a → a) → IO ()
modify r f = do b ← f `φ` read r; write r b
