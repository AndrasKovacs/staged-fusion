
{-# language TemplateHaskell, BlockArguments, RankNTypes #-}

module Gen where

import Up(Up)
import qualified Up

newtype Gen a = Gen {unGen :: forall r. (a -> Up r) -> Up r}

instance Functor Gen where
  fmap f ma = Gen \k -> unGen ma \a -> k (f a)

instance Applicative Gen where
  pure a = Gen \k -> k a
  (<*>) gf ga = Gen \k -> unGen gf \f -> unGen ga \a -> k (f a)

instance Monad Gen where
  return = pure
  (>>=) ga f = Gen \k -> unGen ga \a -> unGen (f a) k

instance MonadFail Gen where
  fail = error

ilet :: Up a -> (Up a -> Gen b) -> Gen b
ilet a f = Gen \k -> [|| let x = $$a in $$(unGen (f [||x||]) k) ||]

ilet' :: Up a -> (Up a -> Gen b) -> Gen b
ilet' a f = Gen \k -> [|| let x = $$a in seq x $$(unGen (f [||x||]) k) ||]

run :: Gen (Up a) -> Up a
run ma = unGen ma id
