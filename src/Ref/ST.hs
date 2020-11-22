--------------------------------------------------------------------
-- | Description : Simple mutable references in the ST monad
--
-- In a concurrent program, 'Ref.ST.Ref' operations may appear out-of-order
--  to another thread, depending on the memory model of the underlying
--  processor architecture.  For example, on x86, loads can move ahead
--  of stores, so in the following example:
--
--  > import Ref.ST
--  > import Control.Monad (unless)
--  > import Control.Concurrent (forkIO, threadDelay)
--  >
--  > maybePrint :: Ref B -> Ref B -> ST s ()
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
--  allows 'Ref.ST.read' to happen before the earlier 'Ref.ST.write'.
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

{-# language PostfixOperators,MagicHash,UnboxedTuples #-}
module Ref.ST (Ref(..), module Ref.ST) where
import qualified "prim" Ref.Boxed as Prim
import ST
import B
import Stock.Eq
import qualified Ref.ST.Lazy as Lazy

data Ref s a = Ref (Prim.Ref s a)
-- ^ A mutable variable in state thread @s@, containing a value of type @a@
--
-- >>> :{
-- runST (do
--     ref <- new "hello"
--     x <- read ref
--     write ref (x ++ "world")
--     read ref )
-- :}
-- "helloworld"


-- | Forces the installed value
new ∷ a → ST s (Ref s a)
new !a = Lazy.new a

-- | Forces the written value
write ∷ Ref s a → a → ST s ()
write r !a = Lazy.write r a

-- | Forces the result
read ∷ Ref s a → ST s a
read r = do !x ← Lazy.read r; η x


-- | Forces the result
modify ∷ Ref s a → (a → a) → ST s ()
modify r f = do !b ← f `φ` read r; write r b
