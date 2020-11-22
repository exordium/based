--------------------------------------------------------------------
-- | Description : Show,Read, and trace values for debugging
--------------------------------------------------------------------
{-# language NoImplicitPrelude,MagicHash #-}
module Debug (Show(..),Read(..), GHC.traceStack,GHC.traceIO, trace, strace) where
import GHC.Show
import GHC.Read
import qualified Debug.Trace as GHC

trace ∷ Show a ⇒ a → a
trace = GHC.traceShowId
strace = GHC.trace
