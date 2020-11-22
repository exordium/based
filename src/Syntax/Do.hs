--------------------------------------------------------------------
-- | Description : Do notation
--
-- see also ApplicativeDo, RecursiveDo, BlockArguments
--------------------------------------------------------------------
module Syntax.Do (module X) where
import GHC.Base as X (Monad(..), Applicative(..), Functor(..))
import Control.Monad.Fix as X (MonadFix(..))
