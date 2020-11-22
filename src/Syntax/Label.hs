--------------------------------------------------------------------
-- | Description : For -XOverloadedLabels
--
-- see https://downloads.haskell.org/ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-OverloadedLabels
--
-- When @OverloadedLabels@ is enabled, if GHC sees an occurrence of
-- the overloaded label syntax @#foo@, it is replaced with
--
-- > fromLabel @"foo" :: alpha
--
-- plus a wanted constraint @IsLabel "foo" alpha@.
--
-- Note that if @RebindableSyntax@ is enabled, the desugaring of
-- overloaded label syntax will make use of whatever @fromLabel@ is in
-- scope.
--------------------------------------------------------------------
module Syntax.Label (IsLabel(..)) where
import GHC.OverloadedLabels

