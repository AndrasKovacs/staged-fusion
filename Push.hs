
{-# language
  TemplateHaskell, TypeFamilies, BlockArguments, RankNTypes, ScopedTypeVariables,
  MagicHash, UnboxedTuples
  #-}

module Push where

import Data.Flat
import IO
import qualified Data.Array.FI as AFI
import qualified Data.Array.FM as AFM

import GHC.Exts

import Pull(Pull(..))
import qualified Pull
import Up (Up)
import qualified Up as U
import Gen

newtype Push a = Push {fold :: forall l. (Up a -> Up l -> Up l) -> Up l -> Up l}

instance Semigroup (Push a) where
  (<>) xs ys = Push \c n -> fold xs c (fold ys c n)

instance Monoid (Push a) where
  mempty = Push \c n -> n

cons :: Up a -> Push a -> Push a
cons a as = Push \c n -> c a (fold as c n)

map :: (Up a -> Up b) -> Push a -> Push b
map f as = Push \c n -> fold as (\a bs -> c (f a) bs) n

pure :: Up a -> Push a
pure a = Push \c n -> c a n

infixl 1 >>=
(>>=) :: Push a -> (Up a -> Push b) -> Push b
(>>=) as f = Push \c n -> fold as (\a bs -> fold (f a) c bs) n

infixl 1 >>
(>>) :: Push a -> Push b -> Push b
(>>) as bs = as Push.>>= \_ -> bs

guard :: Up Bool -> Push ()
guard cond = Push \c n -> U.bool cond (c U.tt n) n

filter :: (Up a -> Up Bool) -> Push a -> Push a
filter f as = Push \c n -> fold as (\a as -> U.bool (f a) (c a as) as) n

foldl :: (Up b -> Up a -> Up b) -> Up b -> Push a -> Up b
foldl f b as =
  [||$$(fold as (\a hyp -> [||\ b -> seq b ($$hyp $$(f [||b||] a)) ||]) [||\ b -> seq b b||]) $$b||]


foldr :: (Up a -> Up b -> Up b) -> Up b -> Push a -> Up b
foldr f b as = fold as f b

sum :: U.Num a => Push a -> Up a
sum = Push.foldl (+) 0

prod :: U.Num a => Push a -> Up a
prod = Push.foldl (*) 1

head :: Push a -> Up a
head as = fold as (\a _ -> a) [||error "head: empty Push"||]

range :: Up Int -> Up Int -> Push Int
range lo hi = Push \c n -> [||
  let go lo hi | lo >= hi  = $$n
               | otherwise = $$(c [||lo||] [||go (lo + 1) hi||])
  in go $$lo $$hi ||]

take :: Up Int -> Push a -> Push a
take i as = Push \c n ->
  [|| $$(fold as (\a hyp -> [|| \(i::Int) ->
                     if i <= 0 then $$n else $$(c a [||$$hyp $!(i-1)||]) ||])
                 [|| \i -> seq i $$n||]) $$i ||]

drop :: Up Int -> Push a -> Push a
drop i as = Push \c n ->
  [|| $$(fold as (\a hyp -> [|| \(i::Int) ->
                     if i <= 0
                       then $$(c a [||$$hyp 0||])
                       else $$hyp $! (i-1) ||])
                 [|| \i -> seq i $$n||]) $$i ||]

fromList :: Up [a] -> Push a
fromList as = Push \c n -> [||
  let go []     = $$n
      go (a:as) = $$(c [||a||] [||go as||])
  in go $$as ||]

toList :: Push a -> Up [a]
toList as = fold as (U.qt2 [||(:)||]) [||[]||]

toList' :: Push a -> Up [a]
toList' as = fold as (\a as -> [|| ((:) $! $$a) $! $$as ||]) [||[]||]

pull :: Pull a -> Push a
pull as = Push \c n -> run do
  Pull.Pull seed step <- as
  Prelude.pure [||
    let go s = seq s ($$(step [||s||] n (\a s -> c a [||go $$s||])))
    in go $$seed ||]

zipWithPull :: (Up a -> Up b -> Up c) -> Push a -> Pull b -> Push c
zipWithPull f as bs = Push \c n -> run do
  Pull.Pull seed step <- bs
  Prelude.pure [|| $$(fold as
         (\a hyp -> [|| \s -> seq s ($$(step [||s||] n (\b s -> c (f a b) [|| $$hyp $$s ||]))) ||])
         [|| \s -> seq s $$n ||])
         $$seed ||]

fromAFI :: Flat a => Up (AFI.Array a) -> Push a
fromAFI as = Push \c n -> [||
  let as' = $$as
      s   = AFI.size as'
      go i s as | i >= s = seq as $$n
                | True   = let x = as AFI.! i; in seq x $$(c [||x||] [||go (i + 1) s as||])
  in seq as' (seq s (go 0 s as')) ||]

data ToAFI a = ToAFI !(AFM.Array a) !Int !(State# RealWorld)

toAFI :: forall a. Flat a => Push a -> Up (AFI.Array a)
toAFI as = [||
  case $$(fold as
     (\a hyp -> [||\l w ->
        let x = $$a; l' = l + 1
        in case seq x (seq l' ($$hyp l' w)) of
          ToAFI marr l w ->
            let l' = l - 1
            in case unIO (AFM.write marr l' x) w of
              (# w, _ #) -> ToAFI marr l' w
        ||])
     [||\l w -> case unIO (AFM.new l) w of (# w, marr #) -> ToAFI marr l w||])
     (0::Int) realWorld# of
    ToAFI arr _ w -> case unIO (AFM.unsafeFreeze arr) w of
      (# _, arr #) -> arr
  ||]
