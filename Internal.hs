{-# language MagicHash #-}

module Internal where

import GHC.Exts
import qualified Data.Array.FM as AFM
import qualified Data.Array.LM as ALM

data W = W !(State# RealWorld)
data ToAFI a = ToAFI !(AFM.Array a) !Int !(State# RealWorld)
data ToALI a = ToALI !(ALM.Array a) !Int !(State# RealWorld)
