{-# language MagicHash #-}
module F32 where
import qualified "prim" F32 as P

type F32# = P.F32
data {-# CTYPE "HsWord" #-} F32 = F32 F32#

