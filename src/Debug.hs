{-# language MagicHash #-}
module Debug (Show(..),Read(..)) where
import GHC.Show
import GHC.Read

{-
traceIO ∷ String → a → a
traceIO msg = do
  withCString "%s\n" \ cfmt → do
    
    -}
--foreign import ccall unsafe "HsBase.h debugBelch2"
  --debugBelch ∷ String.C# → String.C# → IO ()
