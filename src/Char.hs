--------------------------------------------------------------------
-- | Description : Unicode Codepoints
--------------------------------------------------------------------
{-# language MagicHash #-}
module Char (Char(..)) where
import qualified "prim" Char as P
import Stock.Char
import B
import I

(>), (≥), (<), (≤), (≡), (≠),
  gt,ge,lt,le,eq,ne ∷ Char → Char → B
C# a > C# b = B# (a P.> b)
C# a ≥ C# b = B# (a P.≥ b)
C# a < C# b = B# (a P.< b)
C# a ≤ C# b = B# (a P.≤ b) 
C# a ≡ C# b = B# (a P.≡ b)
C# a ≠ C# b = B# (a P.≠ b)
C# a `gt` C# b = B# (P.gt a b)
C# a `ge` C# b = B# (P.ge a b)
C# a `lt` C# b = B# (P.lt a b)
C# a `le` C# b = B# (P.le a b) 
C# a `eq` C# b = B# (a P.≡ b)
C# a `ne` C# b = B# (a P.≠ b)

{-
fromI ∷ I → Char
fromI (I# i) = C# (P.fromI i)
toI ∷ Char → I
toI (C# i) = I (P.toI i)
-}
