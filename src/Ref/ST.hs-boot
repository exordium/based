module Ref.ST where
import qualified "prim" Ref.Boxed as Prim

data Ref s a = Ref (Prim.Ref s a)
