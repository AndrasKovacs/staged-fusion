
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

-- data Int2 = Int2 Int Int deriving Show

-- complex :: AFI.Array Int -> AFI.Array Int -> ALI.Array Int2
-- complex xs ys = $$(
--   PS.toALI $
--   PS.zipWithPull (U.qt2 [||Int2||])

--     (PS.filter U.even $ PS.do {
--         x <- PS.countFrom 1;
--         PS.take 3 (PS.countFrom (x + 1))})

--     (PL.map (\x -> x * x) $
--      PL.filter U.even $
--      PL.take 12 $
--      PL.map (\x -> x * x) $
--      PL.fromAFI [||xs||])
--   )

-- cart :: AFI.Array Int -> AFI.Array Int -> Int
-- cart xs ys = $$(
--   PS.sum $
--   PS.do {
--    x <- PS.fromAFI [|| xs ||];
--    y <- PS.fromAFI [|| ys ||];
--    PS.pure $ x * y}
--   )

-- sumOfSquares :: AFI.Array Int -> Int
-- sumOfSquares xs = $$(
--   PS.sum $
--   PS.map (\x -> x * x) $
--   PS.fromAFI [||xs||])

-- sumOfSquaresEven :: AFI.Array Int -> Int
-- sumOfSquaresEven xs = $$(
--   PS.sum $
--   PS.map (\x -> x * x) $
--   PS.filter U.even $
--   PS.fromAFI [||xs||])

-- dotProduct :: AFI.Array Int -> AFI.Array Int -> Int
-- dotProduct xs ys = $$(
--   PL.sum $
--   PL.zipWith (*)
--     (PL.fromAFI [||xs||])
--     (PL.fromAFI [||ys||])
--   )

-- bindZipWith :: AFI.Array Int -> AFI.Array Int -> Int
-- bindZipWith xs ys = $$(
--   PS.sum $
--   PS.map2 (+)
--     (PS.zipWithPull (+)
--        (PS.fromAFI [||xs||])
--        (PL.fromAFI [||ys||]))
--     (PS.fromAFI [||ys||])
--   )

-- zipWithBind :: AFI.Array Int -> AFI.Array Int -> Int
-- zipWithBind xs ys = $$(
--   PS.sum $
--   flip (PS.zipWithPull (+)) (PL.fromAFI [||xs||]) $
--   PS.map2 (+) (PS.fromAFI [||ys||]) $
--   PS.fromAFI [||xs||]
--   )

-- bindTake :: AFI.Array Int -> AFI.Array Int -> Int
-- bindTake xs ys = $$(
--   PS.sum $
--   PS.take 20000000 $
--   PS.map2 (*) (PS.fromAFI [||ys||]) $
--   PS.fromAFI [||xs||]
--   )
