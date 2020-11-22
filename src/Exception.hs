{-# language MagicHash #-}
module Exception (module X, module Exception) where

import GHC.Exception.Type as X
--import GHC.Err as X (error, errorWithoutStackTrace)
import "prim" Type
import Exception.CallStack
import "prim" Exception



throw ∷ ∀ (r ∷ RuntimeRep) (a ∷ TYPE r) e. Exception e ⇒ e → a
throw e = raise# (toException e)

--error ∷ ∀ (r ∷ RuntimeRep) (a ∷ TYPE r) e. HasCallStack ⇒ e → a
--error s = raise# (

--(⊥) ∷ ∀ (r ∷ RuntimeRep) (a ∷ TYPE r). HasCallStack ⇒ a
--(⊥) = GHC.undefined
