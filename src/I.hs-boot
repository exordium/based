{-# language MagicHash #-}
module I where
import qualified "prim" I as P

type I# = P.I
data {-# CTYPE "HsInt" #-} I = I I#

