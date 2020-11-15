module Maybe where
import Eq
import Ord

data (?) a = Ø | J a deriving (Eq, Ord)

maybe ∷ b → (a → b) → (?) a → b
maybe b0 f = \case {Ø → b0; J a → f a} 

