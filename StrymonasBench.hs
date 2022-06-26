
{-# language TemplateHaskell, BlockArguments, QualifiedDo, Strict #-}

module StrymonasBench where

import Control.Monad

import Up(Up)
import qualified Up as U

import Push(Push)
import qualified Push as PS

import Pull(Pull)
import qualified Pull as PL

import qualified Data.Array.FI as AFI
import qualified Data.Array.LI as ALI

foo :: AFI.Array Int -> AFI.Array Int
foo xs = $$(PS.toAFI $ PS.map (+100) $ PS.fromAFI [||xs||])

-- complex :: AFI.Array Int -> AFI.Array Int -> [U.Pair Int Int]
-- complex !xs !ys = $$(
--   PS.toList $
--   PS.zipWithPull U.pair
--      _

--     (PL.map (\x -> x * x) $
--      PL.filter U.even $
--      PL.take 12 $
--      PL.map (\x -> x * x) $
--      PL.fromAFI [||xs||])
--   )


cart :: AFI.Array Int -> AFI.Array Int -> Int
cart xs ys = $$(
  PS.sum $
  PS.do {
   x <- PS.fromAFI [|| xs ||];
   y <- PS.fromAFI [|| ys ||];
   PS.pure $ x * y}
  )
