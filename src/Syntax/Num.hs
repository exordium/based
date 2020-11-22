--------------------------------------------------------------------
-- | Description : Numeric class using @fromInteger@ for integer literals
--
-- see also NegativeLiterals, NumDecimals, BinaryLiterals, HexLiterals,
-- HexFloatLiterals, NumericUnderscores
--------------------------------------------------------------------
module Syntax.Num
  (Num(..) -- | @fromInteger@ is used for integral literals, and @negate@ desugars unary negation @(- n)@
  ,Fractional(..) -- | @fromRational@ desugars floating point literals
  ) where
import GHC.Num
import GHC.Real
