
{-# language TemplateHaskell, BlockArguments, QualifiedDo, Strict #-}

module Benchmark where

import Control.Monad
import Data.Foldable

import Push(Push)
import Pull(Pull)
import Up(Up)

import qualified Push as PS
import qualified Pull as PL
import qualified Up as U

import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V

import qualified Data.Array.FI as AFI
import qualified Data.Array.LI as ALI

{-
Benchmarks, partly by me, partly borrowed from
   - "Stream Fusion, to Completeness": https://arxiv.org/pdf/1612.06668.pdf

Although it's called "Benchmark.hs", what we test is not the performance, but
instead the quality of generated Core code, where quality is determined by
eyeballing. It would be fun to write actual performance benchmarks and compare
to other fusion libraries.

When it's possible, I write both a push and a pull definition.

I also write Data.List definitions for purely Push definitions. These should
fuse, but in practice they often don't.

Evaluation: I use ghc-9.2.3 -O1 and look at -ddump-simpl. Test cases get the following scores:

  SUCCESS: contains no intermediate structures, closures, unknown function calls
           or unnecessary boxed values. It's reasonably close to a hand-rolled
           fused definition, although there might be some superfluous loop
           variables or other issues with minor performance impact.

  SUCCESS*: fully fused and unboxed, but has some noticeable issues
           regarding code size or structure.

  FAIL: contains intermediate structures or closures or unknown calls.
-}


--------------------------------------------------------------------------------

-- "complex" example from the stream fusion paper
-- SUCCESS
complex :: [Int] -> [Int] -> [(Int, Int)]
complex xs ys = $$(
  PS.toList' $
  PS.zipWithPull (\x y -> [||($$x, $$y)||])

    (PS.filter U.even $ PS.do {
        x <- PS.countFrom 1;
        PS.take 3 (PS.countFrom (x + 1))})

    (PL.map (\x -> x * x) $
     PL.filter U.even $
     PL.take 12 $
     PL.map (\x -> x * x) $
     PL.fromList [||xs||])
  )

-- Sum
--------------------------------------------------------------------------------

-- SUCCESS
sumPush :: [Int] -> Int
sumPush xs = $$(PS.sum $ PS.fromList [||xs||])

-- SUCCESS
sumPull :: [Int] -> Int
sumPull xs = $$(PL.sum $ PL.fromList [||xs||])

-- FAIL: compiles to foldl' (+) 0 without inlining.
sumList1 :: [Int] -> Int
sumList1 = sum

-- SUCCESS
sumList2 :: [Int] -> Int
sumList2 xs = sum xs

-- SUCCESS
sumVec :: Vector Int -> Int
sumVec = V.sum


-- Sum of squares
--------------------------------------------------------------------------------

-- SUCCESS
sumOfSquaresPush :: [Int] -> Int
sumOfSquaresPush xs = $$(
  PS.sum $
  PS.map (\x -> x U.* x) $
  PS.fromList [||xs||])

-- SUCCESS
sumOfSquaresPull :: [Int] -> Int
sumOfSquaresPull xs = $$(
  PL.sum $
  PL.map (\x -> x U.* x) $
  PL.fromList [||xs||])

-- SUCCESS
sumOfSquaresList :: [Int] -> Int
sumOfSquaresList = sum . map (\x -> x * x)

-- SUCCESS
sumOfSquaresList2 :: [Int] -> Int
sumOfSquaresList2 xs = sum $ map (\x -> x * x) xs

sumOfSquaresVec :: Vector Int -> Int
sumOfSquaresVec = V.sum . V.map (\x -> x * x)


-- map
--------------------------------------------------------------------------------

-- SUCCESS
mapPush :: [Int] -> [Int]
mapPush xs =
  $$(PS.toList $ PS.map (*1) $ PS.map (*2) $ PS.map (*3) $ PS.fromList [||xs||])

-- SUCCESS
mapPull :: [Int] -> [Int]
mapPull xs =
  $$(PL.toList $ PL.map (*1) $ PL.map (*2) $ PL.map (*3) $ PL.fromList [||xs||])

-- FAIL: the 3 maps are merged to one, but that last map is not inlined.
mapList :: [Int] -> [Int]
mapList xs = map (*1) $ map (*2) $ map (*3) xs

-- SUCCESS, but there's some dead code being generated for handling errors which
-- are not possible.
mapVec :: Vector Int -> Vector Int
mapVec xs = V.map (*1) $ V.map (*2) $ V.map (*3) xs

-- SUCCESS
mapPull' :: AFI.Array Int -> AFI.Array Int
mapPull' xs = $$(PL.toAFI $ PL.map (*1) $ PL.map (*2) $ PL.map (*3) $ PL.fromAFI [||xs||])


-- sum-map
--------------------------------------------------------------------------------

-- SUCCESS
sumMapPush :: [Int] -> Int
sumMapPush xs = $$(
  PS.sum $
  PS.map (*1) $
  PS.map (*2) $
  PS.map (*3) $
  PS.fromList [||xs||])

-- SUCCESS
sumMapPull :: [Int] -> Int
sumMapPull xs = $$(
  PL.sum $
  PL.map (*1) $
  PL.map (*2) $
  PL.map (*3) $
  PL.fromList [||xs||])

-- SUCCESS
sumMapList :: [Int] -> Int
sumMapList xs =
  sum $
  map (*1) $
  map (*2) $
  map (*3) $
  xs

-- SUCCESS
sumMapVec :: Vector Int -> Int
sumMapVec xs =
  V.sum $
  V.map (*1) $
  V.map (*2) $
  V.map (*3) $
  xs

-- filter
--------------------------------------------------------------------------------

-- SUCCESS
filterPush :: [Int] -> Int
filterPush xs = $$(
  PS.sum $
  PS.filter (U.< 1) $
  PS.filter (U.< 2) $
  PS.filter (U.< 3) $
  PS.fromList [||xs||])

-- SUCCESS*: generates a separate loop for each "filter", thereby yielding much larger code
-- than filterPush, which puts all condition checking in a single loop.
filterPull :: [Int] -> Int
filterPull xs = $$(
  PL.sum $
  PL.filter (U.< 1) $
  PL.filter (U.< 2) $
  PL.filter (U.< 3) $
  PL.fromList [||xs||])

-- SUCCESS
filterList :: [Int] -> Int
filterList xs =
  sum $
  filter (< 1) $
  filter (< 2) $
  filter (< 3) $
  xs

-- SUCCESS
filterVec :: Vector Int -> Int
filterVec xs =
  V.sum $
  V.filter (< 1) $
  V.filter (< 2) $
  V.filter (< 3) $
  xs

-- guard
--------------------------------------------------------------------------------

-- SUCCESS
guardPush :: Int -> Int -> Int
guardPush x y = $$(
  PS.sum $ PS.do
    a <- PS.range [||x||] [||y||]
    PS.guard (U.even a)
    b <- PS.range 0 100
    PS.pure $ a * b
  )

-- FAIL: there's an intermediate list
guardList :: Int -> Int -> Int
guardList x y =
  sum $ do
    a <- [x..y]
    guard $ even a
    b <- [0..100]
    pure $ a * b


-- drop
--------------------------------------------------------------------------------

-- SUCCESS* :
--   Dropping with Push inserts a check on all the elements which are not dropped. The overhead is tiny
--   but it still doesn't look so nice.
dropPush :: Int -> Int -> Int
dropPush x y = $$(
  PS.sum $
  PS.drop 100 $
  PS.map (+10) $
  PS.range [||x||] [||y||]
  )

-- SUCCESS
dropPull :: Int -> Int -> Int
dropPull x y = $$(
  PL.sum $
  PL.drop 100 $
  PL.map (+10) $
  PL.range [||x||] [||y||]
  )

-- FAIL: creates intermediate list
dropList :: Int -> Int -> Int
dropList x y =
  sum $
  drop 100 $
  map (+10) $
  [x..y]

-- SUCCESS with -fspec-constr
-- FAIL    without
dropVec :: Int -> Int -> Int
dropVec x y =
  V.sum $
  V.drop 100 $
  V.map (+10) $
  V.enumFromTo x y


--------------------------------------------------------------------------------

-- SUCCESS
sumOfSquaresEvenPush :: [Int] -> Int
sumOfSquaresEvenPush xs = $$(
  PS.sum $
  PS.map (\x -> x * x) $
  PS.filter U.even $
  PS.fromList [||xs||])

-- SUCCESS
sumOfSquaresEvenPull :: [Int] -> Int
sumOfSquaresEvenPull xs = $$(
  PL.sum $
  PL.map (\x -> x * x) $
  PL.filter U.even $
  PL.fromList [||xs||])

-- SUCCESS
sumOfSquaresEvenList :: [Int] -> Int
sumOfSquaresEvenList xs =
  sum $
  map (\x -> x * x) $
  filter (\x -> $$(U.even [||x||])) $
  xs

-- SUCCESS
sumOfSquaresEvenVec :: Vector Int -> Int
sumOfSquaresEvenVec xs =
  V.sum $
  V.map (\x -> x * x) $
  V.filter (\x -> $$(U.even [||x||])) $
  xs

--------------------------------------------------------------------------------

-- SUCCESS
cart :: [Int] -> [Int] -> Int
cart xs ys =
  $$(PS.sum $ PS.map2 (*) (PS.fromList [|| xs ||]) (PS.fromList [|| ys ||]))

-- SUCCESS
cartList :: [Int] -> [Int] -> Int
cartList xs ys = sum $ liftM2 (*) xs ys

--------------------------------------------------------------------------------

-- SUCCESS
dotProductPull :: [Int] -> [Int] -> Int
dotProductPull xs ys = $$(
  PL.sum $
  PL.zipWith (*)
    (PL.fromList [||xs||])
    (PL.fromList [||ys||])
  )

-- SUCCESS
dotProductPull' :: AFI.Array Int -> AFI.Array Int -> Int
dotProductPull' xs ys = $$(
  PL.sum $
  PL.zipWith (*)
    (PL.fromAFI [||xs||])
    (PL.fromAFI [||ys||])
  )

-- SUCCESS
dotProductVec :: Vector Int -> Vector Int -> Int
dotProductVec xs ys = V.sum $ V.zipWith (*) xs ys

--------------------------------------------------------------------------------

-- SUCCESS
bindAfterZipWith :: [Int] -> [Int] -> Int
bindAfterZipWith xs ys = $$(
  PS.sum $
  PS.map2 (+)
    (PS.zipWithPull (+)
       (PS.fromList [||xs||])
       (PL.fromList [||ys||]))
    (PS.fromList [||ys||])
  )

--------------------------------------------------------------------------------

-- SUCCESS
zipWithAfterBind :: [Int] -> [Int] -> Int
zipWithAfterBind xs ys = $$(
  PS.sum $
  flip (PS.zipWithPull (+)) (PL.fromList [||xs||]) $
  PS.map2 (+) (PS.fromList [||ys||]) $
  PS.fromList [||xs||]
  )

--------------------------------------------------------------------------------

-- SUCCESS
bindTake :: [Int] -> [Int] -> Int
bindTake xs ys = $$(
  PS.sum $
  PS.take 20000000 $
  PS.map2 (*)
    (PS.fromList [||ys||])
    (PS.fromList [||xs||])
  )

-- SUCCESS
bindTakeList :: [Int] -> [Int] -> Int
bindTakeList xs ys =
  sum $
  take 20000000 $
  liftM2 (*) xs ys
