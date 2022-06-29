
{-# language
  TemplateHaskell, RankNTypes, BlockArguments, CPP, ScopedTypeVariables,
  TypeApplications, QualifiedDo, Strict
  #-}

module Notes where

import Control.Monad

import Up(Up)
import qualified Up as U
import Push(Push)
import Pull(Pull)
import qualified Push2 as PS
import qualified Pull2 as PL

import qualified Data.Array.FI as AFI

arrmap :: AFI.Array Int -> AFI.Array Int
arrmap xs = $$(
  PL.toAFI $
  PL.map (+100) $
  PL.map (+100) $
  PL.fromAFI [||xs||])

-- foo :: Int -> Int -> AFI.Array Int
-- foo x y = $$(PS.toAFI $ PS.range [||x||] [||y||])


-- fromafi :: AFI.Array Int -> Int
-- fromafi xs = $$(
--   PL.sum $
--   PL.fromAFI [||xs||]
--   )

-- plafi :: AFI.Array Int -> AFI.Array Int -> Int
-- plafi xs ys = $$(
--   PL.sum $
--   PL.zipWith (*)
--     (PL.fromAFI [||xs||])
--     (PL.fromAFI [||ys||])
--   )

-- foo :: [Int] -> [Int] -> Int
-- foo xs ys = $$(
--   PL.sum $
--   PL.zipWith (*)
--     (PL.fromList [||xs||])
--     (PL.fromList [||ys||])
--   )

-- ps1 :: [Int] -> [Int]
-- ps1 xs = $$(
--   PS.toList' $
--   PS.map (+10) $
--   PS.map (+10) $
--   PS.map (+10) $
--   PS.fromList [||xs||]
--   )

-- list1 :: [Int] -> [Int]
-- list1 xs =
--   map (+10) $
--   map (+10) $
--   map (+10) xs


-- pl2 :: Int -> Int -> Int
-- pl2 x y = $$(
--   PL.sum $
--   PL.map (+20) $
--   PL.map (+10) $
--   PL.filter U.even $
--   PL.range [||x||] [||y||]
--   )

-- ps2 :: Int -> Int -> Int
-- ps2 x y = $$(
--   PS.sum $
--   PS.map (+20) $
--   PS.map (+10) $
--   PS.filter U.even $
--   PS.range [||x||] [||y||]
--   )

-- list2 :: Int -> Int -> Int
-- list2 x y =
--   sum $
--   map (+20) $
--   map (+10) $
--   filter even $
--   [x..y]

-- ps3 :: Int -> Int -> Int
-- ps3 x y = $$(
--   PS.sum $ PS.do
--     a <- PS.range [||x||] [||y||]
--     PS.guard (U.even a)
--     b <- PS.range 0 100
--     PS.pure $ a * b
--   )

-- list3 :: Int -> Int -> Int
-- list3 x y =
--   sum $ do
--     a <- [x..y]
--     guard $ even a
--     b <- [0..100]
--     pure $ a * b

-- ps4 :: [Int] -> [Int]
-- ps4 xs = $$(PS.toList' $ PS.filter U.even $ PS.map (+10) $ PS.fromList [||xs||])

-- list4 :: [Int] -> [Int]
-- list4 xs = filter even $ map (+10) xs


-- zippl :: [Int] -> [Int] -> Int
-- zippl xs ys = $$(
--   PL.sum $ PL.zipWith (*) (PL.map (+10) $ PL.fromList [||xs||])
--                           (PL.filter U.even $ PL.fromList [||ys||]))

-- ziplist :: [Int] -> [Int] -> Int
-- ziplist xs ys = sum $ zipWith (*) (map (+10) xs)
--                                   (filter even ys)


-- zippl2 :: Int -> Int -> Int
-- zippl2 x y = $$(PL.sum $ PL.drop 10 $ PL.drop 10 $ PL.zipWith (*)(PL.range [||x||] [||y||]) (PL.range 0 100))


-- takeps :: Int -> Int
-- takeps x = $$(PS.sum $ PS.take 10 $ PS.range 0 [||x||])

-- dropps :: Int -> Int
-- dropps x = $$(PS.sum $ PS.drop 10 $ PS.range 0 [||x||])


-- revps :: Int -> Int
-- revps x = $$(PS.sum $ PS.reverse $ PS.range 0 [||x||])
