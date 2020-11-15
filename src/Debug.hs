{-# language MagicHash #-}
module Debug (Debug, showsPrec, show, debug)where
import GHC.Show
import Stock.Char
import qualified String
import Stock.IO

type Debug = Show
type String = String.List

debug ∷ Debug a ⇒ a → String
debug = show

{-
traceIO ∷ String → a → a
traceIO msg = do
  withCString "%s\n" \ cfmt → do
    
    -}
--foreign import ccall unsafe "HsBase.h debugBelch2"
  --debugBelch ∷ String.C# → String.C# → IO ()
