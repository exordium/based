{-# language MagicHash,UnboxedTuples #-}
module Ref.IO where
import qualified Ref.ST as ST
import "prim" Coerce
import qualified "prim" Ref.Boxed as Prim
import "this" IO
import "prim" IO (type (☸))
import B
import Stock.Eq
import "this" ST (ST(..))

newtype Ref a = Ref# (ST.Ref (☸) a) deriving newtype Eq
-- ^ A mutable variable in state thread @s@, containing a value of type @a@
--
-- >>> :{
-- runIO (do
--     ref <- new "hello"
--     x <- read ref
--     write ref (x ++ "world")
--     read ref )
-- :}
-- "helloworld"
pattern Ref ∷ Prim.Ref (☸) a → Ref a
pattern Ref p = Ref# (ST.Ref p)
(≡) ∷ Ref a → Ref a → B
(≡) = (==)

new ∷ a → IO (Ref a)
new a = do p ← fromST (ST.new a); η (Ref# p)

read ∷ Ref a → IO a
read (Ref# r) = fromST do ST.read r

write ∷ Ref a → a → IO ()
write (Ref# r) a = fromST do ST.write r a
