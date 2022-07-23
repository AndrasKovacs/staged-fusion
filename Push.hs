
{-# language
  BlockArguments,
  FlexibleInstances,
  MagicHash,
  QualifiedDo,
  RankNTypes,
  TemplateHaskell,
  UnboxedTuples
  #-}

{-|
Push-based iterators.

- They support efficient nested looping (a monad-like "bind" operation) and
  appending/concatenation.

- We can't zip two `Push`-es together. However, we can zip a `Push` and a `Pull`
  to get a new `Push`. If you find yourself wanting to zip, check if one of
  the arguments can be written as a `Pull`. If both arguments are essentially
  `Push`, then you have to pick one to convert to `Pull`.

- Conversion from `Push` to `Pull` necessarily has some overhead. `toPull`
  gather all results in an intermediate lazy list. `toPull'` gathers results in
  an array. `toPullFlat` gathers results in a flat bytearray. Only `toPull`
  can be used to convert infinite `Push`-es.

- `drop` is reasonably efficient, but it inserts a condition check to the
  processing of all of the /kept/ values. In principle, GHC `-O2` can remove the
  overhead, but usually it doesn't. It is preferred to use `Pull` for dropping,
  where there is zero processing overhead on the kept values.
-}

module Push where

import GHC.Exts
import Data.Flat
import IO
import qualified Data.Array.FI as AFI
import qualified Data.Array.FM as AFM
import qualified Data.Array.LI as ALI
import qualified Data.Array.LM as ALM

import Pull(Pull)
import qualified Pull as PL
import Up (Up)
import qualified Up as U
import Gen
import Internal

data Push' a = Push' {
  len  :: Maybe (Up Int),
  fold :: forall l. (Up a -> Up l -> Up l) -> Up l -> Up l}

type Push a = Gen (Push' a)

foldPush :: Push a -> (Up a -> Up l -> Up l) -> Up l -> Up l
foldPush as c n = unGen as \as -> fold as c n

instance Semigroup (Push a) where
  (<>) xs ys = do
    xs <- xs
    ys <- ys
    return $ Push' ((+) <$> len xs <*> len ys) (\c n -> fold xs c (fold ys c n))

instance Monoid (Push a) where
  mempty = return $ Push' (Just 0) (\c n -> n)

cons :: Up a -> Push a -> Push a
cons a as = do
  as <- as
  return $ Push' ((+1) <$> len as) (\c n -> c a (fold as c n))

map :: (Up a -> Up b) -> Push a -> Push b
map f as = do
  as <- as
  return $ Push' (len as) (\c n -> fold as (\a bs -> c (f a) bs) n)

pure :: Up a -> Push a
pure a = return $ Push' (Just 1) (\c n -> c a n)

infixl 1 >>=
(>>=) :: Push a -> (Up a -> Push b) -> Push b
(>>=) as f = do
  as <- as
  return $ Push' Nothing \c n -> fold as (\a bs -> foldPush (f a) c bs) n

concatMap :: (Up a -> Push b) -> Push a -> Push b
concatMap f as = as Push.>>= f

map2 :: (Up a -> Up b -> Up c) -> Push a -> Push b -> Push c
map2 f as bs = Push.do {a <- as; b <- bs; Push.pure $ f a b}

map3 :: (Up a -> Up b -> Up c -> Up d) -> Push a -> Push b -> Push c -> Push d
map3 f as bs cs = Push.do {a <- as; b <- bs; c <- cs; Push.pure $ f a b c}

infixl 1 >>
(>>) :: Push a -> Push b -> Push b
(>>) as bs = as Push.>>= \_ -> bs

guard :: Up Bool -> Push ()
guard cond = return $ Push' Nothing \c n -> U.bool cond (c U.tt n) n

filter :: (Up a -> Up Bool) -> Push a -> Push a
filter f as = return $ Push' Nothing \c n -> foldPush as (\a as -> U.bool (f a) (c a as) as) n

foldl :: (Up b -> Up a -> Up b) -> Up b -> Push a -> Up b
foldl f b as =
  [||$$(foldPush as (\a hyp -> [||\ b -> seq b ($$hyp $$(f [||b||] a)) ||]) [||\ b -> seq b b||]) $$b||]

foldr :: (Up a -> Up b -> Up b) -> Up b -> Push a -> Up b
foldr f b as = foldPush as f b

sum :: U.Num a => Push a -> Up a
sum = Push.foldl (+) 0

prod :: U.Num a => Push a -> Up a
prod = Push.foldl (*) 1

head :: Push a -> Up a
head as = foldPush as (\a _ -> a) [||error "head: empty Push"||]

range :: Up Int -> Up Int -> Push Int
range lo hi = return $ Push' (Just (U.max 0 (hi - lo))) \c n -> [||
  let go lo hi | (lo :: Int) >= (hi :: Int)  = $$n
               | otherwise = $$(c [||lo||] [||go (lo + 1) hi||])
  in go $$lo $$hi ||]

-- | Starting value, step, continuing condition.
for :: Up a -> (Up a -> Up a) -> (Up a -> Up Bool) -> Push a
for start step cont = return $ Push' Nothing \c n -> [||
  let go s | $$(cont [||s||]) = $$(c [||s||] [||go $$(step [||s||])||])
           | otherwise        = $$n
  in go $$start
  ||]

countFrom :: Up Int -> Push Int
countFrom lo = return $ Push' Nothing \c n -> [||
  let go i = seq (i::Int) $$(c [||i||] [||go (i + 1)||])
  in go $$lo ||]

countFromStep :: Up Int -> Up Int -> Push Int
countFromStep lo step = return $ Push' Nothing \c n -> [||
  let go i step = seq (i::Int) (seq (step :: Int) $$(c [||i||] [||go (i + step) step||]))
  in go $$lo $$step ||]

take :: Up Int -> Push a -> Push a
take i as = do
  as <- as
  return $ Push' (U.max <$> len as <*> Just i) \c n ->
    [|| $$(fold as (\a hyp -> [|| \i ->
                       if (i::Int) <= 0 then $$n else $$(c a [||$$hyp $!(i-1)||]) ||])
                   [|| \i -> seq i $$n||]) $$i ||]

drop :: Up Int -> Push a -> Push a
drop i as = do
  as <- as
  return $ Push' (U.max 0 <$> ((-) <$> len as <*> Just i)) \c n ->
    [|| $$(fold as (\a hyp -> [|| \i ->
                       if (i :: Int) <= 0
                         then $$(c a [||$$hyp 0||])
                         else $$hyp $! (i-1) ||])
                   [|| \i -> seq i $$n||]) $$i ||]

fromList :: Up [a] -> Push a
fromList as = return $ Push' (Just [||length $$as||]) \c n -> [||
  let go []     = $$n
      go (a:as) = $$(c [||a||] [||go as||])
  in go $$as ||]

toList :: Push a -> Up [a]
toList as = foldPush as (U.qt2 [||(:)||]) [||[]||]

toList' :: Push a -> Up [a]
toList' as = foldPush as (\a as -> [|| ((:) $! $$a) $$as ||]) [||[]||]

toList'' :: Push a -> Up [a]
toList'' as = foldPush as (\a as -> [|| ((:) $! $$a) $! $$as ||]) [||[]||]

-- | Lazily convert a `Push` to a `Pull`. Works on infinite input.
toPull :: Push a -> Pull a
toPull = PL.fromList . Push.toList

-- | Convert a `Push` to a `Pull` by gathering values in an array. Only works on finite
--   input.
toPull' :: Push a -> Pull a
toPull' = PL.fromALI . Push.toALI

-- | Convert a `Push` to a `Pull` by gathering values in a flat array. Only works on finite
--   input.
toPullFlat :: Flat a => Push a -> Pull a
toPullFlat = PL.fromAFI . Push.toAFI

fromPull :: Pull a -> Push a
fromPull as = do
  PL.Pull' len seed step <- as
  return $ Push' len \c n ->
    [|| let go s = seq s ($$(step [||s||] n (\a s -> c a [||go $$s||])))
        in go $$seed ||]

zipWithPull :: (Up a -> Up b -> Up c) -> Push a -> Pull b -> Push c
zipWithPull f as bs = do
  Push' len foldas <- as
  PL.Pull' len' seed step <- bs
  return $ Push' (U.min <$> len <*> len') \c n ->
    [|| $$(foldas
         (\a hyp -> [|| \s -> seq s ($$(step [||s||] n (\b s -> c (f a b) [|| $$hyp $$s ||]))) ||])
         [|| \s -> seq s $$n ||])
         $$seed ||]


-- Primitive array conversions
--------------------------------------------------------------------------------

fromAFI :: Flat a => Up (AFI.Array a) -> Push a
fromAFI as =
  ilet' as \as ->
  ilet' [||AFI.size $$as||] \size ->
    return $ Push' (Just size) \c n -> [||
       let go i s as | (i::Int) >= s = seq as $$n
                     | True          = let x = as AFI.! i; in seq x $$(c [||x||] [||go (i + 1) s as||])
       in go 0 $$size $$as ||]

fromALI :: Up (ALI.Array a) -> Push a
fromALI as =
  ilet' as \as ->
  ilet' [||ALI.size $$as||] \size ->
  return $ Push' (Just size) \c n -> [||
    let go i s as | (i :: Int) >= s = seq as $$n
                  | True            = let x = as ALI.! i; in seq x $$(c [||x||] [||go (i + 1) s as||])
    in go 0 $$size $$as ||]

toAFI :: forall a. Flat a => Push a -> Up (AFI.Array a)
toAFI as = run do
  as <- as
  case len as of

    Nothing -> return [||
       AFI.Array (runRW# \w ->
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
            (0::Int) w of
              ToAFI arr _ w -> case unIO (AFM.unsafeFreeze arr) w of
                (# _, AFI.Array arr #) -> arr)
       ||]

    Just len ->
      return [||
        let cont len =
              AFI.Array (runRW# \w -> case unIO (AFM.new len) w of
               (# w, marr #) -> case $$(fold as
                   (\a hyp -> [|| \i w -> case unIO (AFM.write marr i $$a) w of
                     (# w, _ #) -> $$hyp (i + 1) w
                     ||])
                   [|| \i w -> W w ||]
                  )(0::Int) w of
                    W w -> case unIO (AFM.unsafeFreeze marr) w of
                      (# _, AFI.Array arr #) -> arr)
        in cont $! $$len
       ||]


toALI :: forall a. Push a -> Up (ALI.Array a)
toALI as = run do
  as <- as
  case len as of

    Nothing -> return [||
       ALI.Array (runRW# \w ->
         case $$(fold as
                    (\a hyp -> [||\l w ->
                       let x = $$a; l' = l + 1
                       in case seq x (seq l' ($$hyp l' w)) of
                         ToALI marr l w ->
                           let l' = l - 1
                           in case unIO (ALM.write marr l' x) w of
                             (# w, _ #) -> ToALI marr l' w
                       ||])
                    [||\l w -> case unIO (ALM.new l undefined) w of (# w, marr #) -> ToALI marr l w||])
            (0::Int) w of
              ToALI arr _ w -> case unIO (ALM.unsafeFreeze arr) w of
                (# _, ALI.Array arr #) -> arr)
       ||]

    Just len ->
      return [||
        let cont len =
              ALI.Array (runRW# \w -> case unIO (ALM.new len undefined) w of
               (# w, marr #) -> case $$(fold as
                   (\a hyp -> [|| \i w -> case unIO (ALM.write marr i $$a) w of
                     (# w, _ #) -> $$hyp (i + 1) w
                     ||])
                   [|| \i w -> W w ||]
                  )(0::Int) w of
                    W w -> case unIO (ALM.unsafeFreeze marr) w of
                      (# _, ALI.Array arr #) -> arr)
        in cont $! $$len
       ||]
