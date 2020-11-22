{-# language MagicHash #-}
module Ref.IO where
import qualified Ref.ST as ST
import "prim" IO


newtype Ref a = Ref# (ST.Ref (☸) a)
