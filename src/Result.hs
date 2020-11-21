module Result where
import Debug
import Stock.Eq
import Stock.Ord
import qualified Syntax
import Prelude (ι)

data e ! a = Err e | Ok a deriving (Show, Read, Eq, Ord)
either ∷ (e → b) → (a → b) → e ! a → b
either err ok = \case Err e → err e; Ok a → ok a
catch ∷ (e → a) → e ! a → a
catch = (`either` ι)

φ ∷ (a → b) → e ! a → e ! b
φ f = \case Err e → Err e; Ok a → Ok (f a)
instance Syntax.Functor ((!) e) where
  fmap = φ
  (<$) a = \case Err e → Err e; Ok _ → Ok a

instance Syntax.Applicative ((!) e) where
  pure = Ok
  Ok f <*> Ok a = Ok (f a)
  Err e <*> _ = Err e
  _ <*> Err e = Err e
  Ok{} *> b = b
  Err e *> _ = Err e

μ ∷ e ! (e ! a) → e ! a; {-# inline μ #-}
μ = (Syntax.>>= ι)
instance Syntax.Monad ((!) e) where
  m >>= f = case m of Err e → Err e; Ok a → f a
  (>>) = (Syntax.*>)
