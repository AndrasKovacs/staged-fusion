
{-# language
  TemplateHaskell, TypeFamilies, BlockArguments,
  MagicHash, RankNTypes, UnboxedTuples, GADTs #-}

module Pull where

import Data.Flat
import IO
import qualified Data.Array.FI as AFI
import qualified Data.Array.FM as AFM

import Up(Up)
import qualified Up as U
import Gen

import GHC.Exts
import GHC.Int


type Step a s = forall r. Up r -> (Up a -> Up s -> Up r) -> Up r
data Pull' a = forall s. Pull (Up s) (Up s -> Step a s)
type Pull a = Gen (Pull' a)

drop# :: forall s a. Up Int -> (Up s -> Step a s) -> Up s -> Gen (Up s)
drop# n step s = Gen \ret -> [||
  let go n s | n <= (0::Int) = seq s $$(ret [||s||])
             | otherwise     = $$(step [||s||] (ret [||s||]) (\_ s -> [||go (n - 1) $$s||]))
  in go $$n $$s ||]

drop :: Up Int -> Pull a -> Pull a
drop n pa = do
  Pull seed step <- pa
  seed <- drop# n step seed
  pure $ Pull seed step

map :: (Up a -> Up b) -> Pull a -> Pull b
map f pa = do
  Pull seed step <- pa
  pure $ Pull seed \s stop yield -> step s stop \a s -> yield (f a) s

range :: Up Int -> Up Int -> Pull Int
range lo hi = pure $
  Pull (U.pair lo hi) \s stop yield ->
    U.bindPair s \curr hi ->
      U.bool (curr U.>= hi)
             stop
             (yield curr (U.pair (curr U.+ 1) hi))

rangeStep :: Up Int -> Up Int -> Up Int -> Pull Int
rangeStep lo hi step = pure $ Pull (U.tup3 lo hi step) \s stop yield ->
  U.bindTup3 s \lo hi step ->
    U.bool ((lo :: Up Int) U.>= (hi :: Up Int))
           stop
           (yield lo (U.tup3 (lo U.+ (step :: Up Int)) hi step))

zipWith :: (Up a -> Up b -> Up c) -> Pull a -> Pull b -> Pull c
zipWith f pa pb = do
  Pull seed step   <- pa
  Pull seed' step' <- pb
  pure $
    Pull
       (U.pair seed seed')
       (\abs stop yield -> U.bindPair abs \as bs ->
          step as stop
               (\a as -> step' bs stop
                              (\b bs -> yield (f a b) (U.pair as bs)))
       )

find# :: (Up a -> Up Bool) -> (Up s -> Step a s) -> Up s -> Step a s
find# f step s stop yield = [||
   let go s = $$(step [||s||] stop (\a s -> U.bool (f a) (yield a s) [||go $$s||]))
   in  go $$s ||]

filter :: (Up a -> Up Bool) -> Pull a -> Pull a
filter f pa = (\(Pull seed step) -> Pull seed (find# f step)) <$> pa

take :: Up Int -> Pull a -> Pull a
take n pa = do
  Pull seed step <- pa
  pure $ Pull (U.pair seed n) \s stop yield -> [||
          case $$s of
            U.Pair s n ->
              if (n ::Int) <= 0
                 then $$stop
                 else $$(step [||s||] stop (\a s -> yield a (U.pair s ([||n||]-1))))
          ||]

foldl :: (Up b -> Up a -> Up b) -> Up b -> Pull a -> Up b
foldl f b pa = run do
  Pull seed step <- pa
  pure [||
    let go s b = seq b $$(step [||s||] [||b||] (\a s -> [|| go $$s $$(f [||b||] a) ||]))
    in go $$seed $$b ||]

sum :: (U.Num a) => Pull a -> Up a
sum = Pull.foldl (+) 0

product :: (U.Num a) => Pull a -> Up a
product = Pull.foldl (*) 0

takeWhile :: (Up a -> Up Bool) -> Pull a -> Pull a
takeWhile f pa = do
  Pull seed step <- pa
  pure $ Pull seed \s stop yield -> step s stop \a s -> U.bool (f a) (yield a s) stop

dropWhile# :: (Up a -> Up Bool) -> (Up s -> Step a s) -> Up s -> Gen (Up s)
dropWhile# f step s = Gen \ret -> [||
   let go s = $$(step [||s||] (ret [||s||]) (\a s -> U.bool (f a) [||go $$s||] (ret s)))
   in  go $$s ||]

dropWhile :: (Up a -> Up Bool) -> Pull a -> Pull a
dropWhile f pa = do
  Pull seed step <- pa
  seed <- dropWhile# f step seed
  pure $ Pull seed step

fromList :: Up [a] -> Pull a
fromList as = pure $
  Pull as \s stop yield ->
    [|| case $$s of []   -> $$stop
                    a:as -> $$(yield [||a||] [||as||]) ||]

length :: Pull a -> Up Int
length = Pull.foldl (\acc _ -> acc + 1) 0

toList :: Pull a -> Up [a]
toList pa = run do
  Pull seed step <- pa
  pure [|| let go s = seq s ($$(step [||s||] [||[]||] (\ a s -> [|| $$a : go $$s ||])))
           in go $$seed ||]

toList' :: Pull a -> Up [a]
toList' pa = run do
  Pull seed step <- pa
  pure [|| let go s = seq s ($$(step [||s||] [||[]||] (\ a s -> [|| ((:) $! $$a) $! (go $$s) ||])))
           in go $$seed ||]

fromAFI :: forall a. Flat a => Up (AFI.Array a) -> Pull a
fromAFI as =
  ilet' as \as ->
  ilet' [|| AFI.size $$as ||] \size ->
  pure $ Pull (U.tup3 as (0 :: Up Int) size) \s stop yield ->
    [|| case $$s of
          U.Tup3 as i size -> if i < (size ::Int)
            then let a = as AFI.! i
                 in seq a $$(yield [||a||] (U.tup3 [||as||] ([||i||] + 1) [||size||]))
            else $$stop
     ||]

data ToAFI a = ToAFI !(AFM.Array a) !Int !(State# RealWorld)

toAFI :: forall a. Flat a => Pull a -> Up (AFI.Array a)
toAFI as = run do
  Pull seed step <- as
  pure [||
    let go s len w = $$(step [||s||]
         [|| case unIO (AFM.new len) w of
               (# w, marr #) -> ToAFI marr len w
          ||]
         \a s -> [||
            let x = $$a;
            in case go $$s (len + 1) w of
              ToAFI marr len w ->
                let len' = len - 1
                in case unIO (AFM.write marr len' x) w of
                  (# w, _ #) -> ToAFI marr len' w
                   ||]
         )
    in case go $$seed 0 realWorld# of
      ToAFI arr _ w -> case unIO (AFM.unsafeFreeze arr) w of
        (# _, arr #) -> arr
    ||]
