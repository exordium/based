--------------------------------------------------------------------
-- | Description : List literals with -XOverloadedLists
--
-- see https://downloads.haskell.org/ghc/latest/docs/html/users_guide/glasgow_exts.html#overloaded-lists
--------------------------------------------------------------------
module Syntax.List (module X) where
import GHC.Exts as X (IsList(..))
