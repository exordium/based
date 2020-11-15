{-# language MagicHash #-}
module U where
import qualified "prim" U as P

type U# = P.U
data {-# CTYPE "HsWord" #-} U = U U#

