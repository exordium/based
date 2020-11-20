module Syntax.Num
  (Num(..) -- | @fromInteger@ is used for integral literals, and @negate@ desugars unary negation @(- n)@
  ,Fractional(..) -- | @fromRational@ desugars floating point literals
  ) where
import GHC.Num
import GHC.Real
