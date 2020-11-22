--------------------------------------------------------------------
-- | Description : Simple mutable references specialized to the IO monad
--
-- In a concurrent program, 'Ref.IO.Ref' operations may appear out-of-order
--  to another thread, depending on the memory model of the underlying
--  processor architecture.  For example, on x86, loads can move ahead
--  of stores, so in the following example:
--
--  > import Ref.IO
--  > import Control.Monad (unless)
--  > import Control.Concurrent (forkIO, threadDelay)
--  >
--  > maybePrint :: Ref B -> Ref B -> IO ()
--  > maybePrint myRef yourRef = do
--  >   writeRef myRef True
--  >   yourVal <- readRef yourRef
--  >   unless yourVal $ putStrLn "critical section"
--  >
--  > main :: IO ()
--  > main = do
--  >   r1 <- newRef False
--  >   r2 <- newRef False
--  >   forkIO $ maybePrint r1 r2
--  >   forkIO $ maybePrint r2 r1
--  >   threadDelay 1000000
--
--  it is possible that the string @"critical section"@ is printed
--  twice, even though there is no interleaving of the operations of the
--  two threads that allows that outcome.  The memory model of x86
--  allows 'Ref.IO.read' to happen before the earlier 'Ref.IO.write'.
--
--  The implementation is required to ensure that reordering of memory
--  operations cannot cause type-correct code to go wrong.  In
--  particular, when inspecting the value read from an 'Ref', the
--  memory writes that created that value must have occurred from the
--  point of view of the current thread.
--
--  'atomicModifyRef' acts as a barrier to reordering.  Multiple
--  'atomicModifyRef' operations occur in strict program order.  An
--  'atomicModifyRef' is never observed to take place ahead of any
--  earlier (in program order) 'Ref' operations, or after any later
--  'Ref' operations.
--------------------------------------------------------------------

{-# language MagicHash,UnboxedTuples #-}
module Ref.IO (Ref(Ref#,Ref)) where
import qualified Ref.ST as ST
import "prim" Coerce
import qualified "prim" Ref.Boxed as Prim
import "this" IO
import "prim" IO (type (☸))
import B
import Stock.Eq
import "this" ST (ST(..))

newtype Ref a = Ref# (ST.Ref (☸) a) deriving newtype Eq
-- ^ A mutable variable containing a value of type @a@
--
-- >>> :{
--  do ref ← new "hello"
--     x ← read ref
--     write ref (x ++ "world")
--     read ref
-- :}
-- "helloworld"
pattern Ref ∷ Prim.Ref (☸) a → Ref a
pattern Ref p = Ref# (ST.Ref p)
{-# complete Ref #-}
(≡) ∷ Ref a → Ref a → B
(≡) = (==)

new ∷ a → IO (Ref a)
new a = do p ← fromST (ST.new a); η (Ref# p)

read ∷ Ref a → IO a
read (Ref# r) = fromST do ST.read r

write ∷ Ref a → a → IO ()
write (Ref# r) a = fromST do ST.write r a
