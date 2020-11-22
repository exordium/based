--------------------------------------------------------------------
-- | Description : Typeclass for total orders
--------------------------------------------------------------------
module Ord (Ord(..), module Ord) where
import B
import Stock.Ord

(≤), (≥) ∷ Ord a ⇒ a → a → B
(≤) = (<=); (≥) = (>=)
{-# inline (≤) #-};{-# inline (≥) #-}
