module Exception.Stack (CallStack(Empty,Push,Freeze,CallStack)
                       ,HasCallStack,SrcLoc(..)) where
import GHC.Stack.Types
-- TODO: rename/strip SrcLoc
import Stock.Char

-- | The empty 'CallStack'.
pattern Empty ∷ CallStack
pattern Empty = EmptyCallStack
-- | Push a call-site onto the stack.
-- This function has no effect on a frozen 'CallStack'.
pattern Push ∷ [Char] → SrcLoc → CallStack → CallStack
pattern Push s l c = PushCallStack s l c
pattern Freeze ∷ CallStack → CallStack
-- | Freeze a call-stack, preventing any further call-sites from being appended.
pattern Freeze cs = FreezeCallStack cs
{-# complete Empty , Push, Freeze #-}
pattern CallStack ∷ [([Char], SrcLoc)] → CallStack
pattern CallStack cs ← (getCallStack → cs)  where CallStack = fromCallSiteList
{-# complete CallStack #-}

