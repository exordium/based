--------------------------------------------------------------------
-- | Description : Typeclass for equality
--------------------------------------------------------------------
module Eq (Eq(..), module Eq) where
import B
import Stock.Eq as X

(≡), (≠) ∷ Eq a ⇒ a → a → B
(≡) = (==); (≠) = (/=)
{-# inline (≡) #-};{-# inline (≠) #-}
