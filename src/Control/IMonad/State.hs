{-# LANGUAGE GADTs         #-}
{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE TypeOperators #-}

module Control.IMonad.State where

import           Control.Category.Index
import           Control.IMonad.Do
import           Control.IMonad.Identity
import           Control.IMonad.Restrict
import           Unsafe.Coerce

-- Existential product

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

-- State Monad

-- I have no idea how to make a proper indexed monad transformer
-- version of this. Or rather, I'd have to conflate the index erasure,
-- the pair, and the monad all together and that seems wrong.
--
-- > newtype (a :* b) i = P (a i, b i)
-- > data P m a b where
-- >   Pm :: m (a :* b) i -> P m a b
--
-- How ugly!

newtype State s a i =
  State { runState :: s i -> (a :* s) }

instance IFunctor (State s) where
  fmapI phi m = State $ \si ->
    case runState m si of
      P ai si -> P (phi ai) si

instance IMonad (State s) where
  returnI ai = State $ \si -> P ai si
  bindI f ma = State $ \si ->
    case runState ma si of
      P ai si -> runState (f ai) si

state :: (s i -> (a j, s j)) -> State s a i
state f = State $ \s -> let (a, s') = f s in P a s'

runState' :: State b (a := i) j -> b j -> (a, b i)
runState' m s0 = on1 (runState m s0)

evalState' :: State b (c := i) j -> b j -> c
evalState' m = fst . runState' m

execState' :: State b (c := i) j -> b j -> b i
execState' m = snd . runState' m

mapState :: ((a :* s) -> (b :* s)) -> State s a i -> State s b i
mapState phi m = State (phi . runState m)

mapState' :: (forall i . a i -> s i -> (b j, s j)) -> State s a i -> State s b i
mapState' phi m = State (mapStar phi . runState m)

withState :: (s :-> s) -> State s a i -> State s a i
withState phi m = State $ \s ->
  case runState m s of
    P a s' -> P a (phi s')

get :: State s s i
get = State (\s -> P s s)

put :: s i -> State s (() := i) j
put s = State (\_ -> P (V ()) s)

modify :: (s i -> s j) -> State s (() := j) i
modify phi = State $ \s -> P (V ()) (phi s)

gets :: (s i -> a i) -> State s a i
gets f = State $ \s -> P (f s) s
