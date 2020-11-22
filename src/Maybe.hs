--------------------------------------------------------------------
-- | Description : Monad for optional values
--------------------------------------------------------------------
module Maybe where
import Eq
import Ord
import qualified Syntax

data (?) a = Ø | J a deriving (Eq, Ord)

maybe ∷ b → (a → b) → (?) a → b
maybe b0 f = \case {Ø → b0; J a → f a} 

φ ∷ (a → b) → (?) a → (?) b
φ f = \case {Ø → Ø; J a → J (f a)}

μ ∷ (?) ((?) a) → (?) a
μ = \case {Ø → Ø; J Ø → Ø; J (J a) → J a}

instance Syntax.Functor (?) where
  fmap = φ
  a <$ _ = J a
instance Syntax.Applicative (?) where
  pure = J
  f' <*> a' = do f ← f'; a ← a'; J (f a)
instance Syntax.Monad (?) where
  a >>= f = maybe Ø f a
  return = J
