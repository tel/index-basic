{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TypeOperators    #-}

module Control.IMonad.Writer where

import           Control.Arrow
import           Control.Category
import           Control.Category.Index
import           Control.IMonad.Do
import           Control.IMonad.Identity
import           Control.IMonad.Restrict
import           Data.Indexed.Product
import           Prelude                 hiding (id, (.))

newtype Writer w a i = Writer { runWriter :: (a :* w i) }

instance IFunctor (Writer w) where
  fmapI phi w = case runWriter w of
    P aj wij -> writer (phi aj) wij

instance Category w => IMonad (Writer w) where
  returnI ai = writer ai id
  bindI f w = case runWriter w of
    P ai wi -> case runWriter (f ai) of
      P bj wj -> writer bj (wj . wi)

writer :: a j -> w i j -> Writer w a i
writer aj wij = Writer (P aj wij)

execWriter :: Writer w (a := i) i1 -> w i1 i
execWriter = snd . on1 . runWriter

mapWriter :: (forall k. a k -> w i k -> (b l, v j l)) -> Writer w a i -> Writer v b j
mapWriter phi = Writer . mapStar phi . runWriter

tell :: w i j -> Writer w (() := j) i
tell = writer (V ())
