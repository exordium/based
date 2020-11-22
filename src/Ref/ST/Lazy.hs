{-# language MagicHash,UnboxedTuples #-}
module Ref.ST.Lazy where
import qualified "prim" Ref.Boxed as Prim
import ST
import B
import Stock.Eq
import {-# source #-} Ref.ST

(≡) ∷ Ref s a → Ref s a → B
Ref p ≡ Ref q = B# (p Prim.≡ q)
instance Eq (Ref s a) where (==) = (≡)

new ∷ a → ST s (Ref s a)
new a = ST# \ s → case Prim.new a s of (# s', r #) → (# s', Ref r #)

read ∷ Ref s a → ST s a
read (Ref r) = ST# do Prim.read r

write ∷ Ref s a → a → ST s ()
write (Ref r) a = ST# \ s → case Prim.write r a s of s' → (# s', () #)

modify ∷ Ref s a → (a → a) → ST s ()
modify r f = do b ← f `φ` read r; write r b
