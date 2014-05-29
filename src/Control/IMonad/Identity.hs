
module Control.IMonad.Identity where

import           Control.IMonad.Do

newtype Identity a i = Identity { runIdentity :: a i }

instance IFunctor Identity where
  fmapI f (Identity a) = Identity (f a)

instance IMonad Identity where
  returnI = Identity
  bindI f (Identity x) = f x
