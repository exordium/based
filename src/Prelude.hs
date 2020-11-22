module Prelude (module X, module Prelude) where

import Stock.Eq as X
import Debug (Show(..),Read(..))

ι ∷ a → a
ι x = x

(∘) ∷ (x → b) → (a → x) → a → b
f ∘ g = \x → f (g x)

(⁏) ∷ (a → x) → (x → b) → a → b
f ⁏ g = \x → g (f x)


infixl 0 `on`
on ∷ (x → x → b) → (a → x) → a → a → b
on f g = \x y → f (g x) (g y)

(⊳) ∷ a → (a → b) → b
x ⊳ f = f x

κ ∷ a → b → a
κ a _ = a


-- ⊢ fold
-- ⊨ traverse
