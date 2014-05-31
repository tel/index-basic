{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE TypeOperators             #-}

module Data.Indexed.Product where

import           Control.IMonad.Do
import           Unsafe.Coerce

data a :* b where
  P :: a i -> b i -> a :* b

-- Since we've forgotten the index, we can no longer consider the two
-- elements of the pair individually---UNLESS one of them is
-- restricted to exist only at a single index. That's sufficient
-- information to (a) pull out the value at that index (as always)
-- and, more interestingly, (b) know the index of the other part.

-- Haskell can't prove this itself, so we'll just let it know this is
-- an ok move.

on1 :: (a := i) :* b -> (a, b i)
on1 (P (V a) b) = (a, unsafeCoerce b)

on2 :: a :* (b := i) -> (a i, b)
on2 (P a (V b)) = (unsafeCoerce a, b)

mapStar :: (forall i . a i -> b i -> (c j, d j)) -> a :* b -> c :* d
mapStar phi (P a b) = let (c, d) = phi a b in P c d
