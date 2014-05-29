{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE TypeOperators             #-}

module Control.IMonad.Cont where

import           Control.Category.Index
import           Control.IMonad.Do
import           Control.IMonad.Identity
import           Control.IMonad.Trans
import           Prelude                 hiding (return, (>>), (>>=))

newtype ContT r (m :: (* -> *) -> (* -> *)) a i =
  ContT { runContT :: (a :-> m r) -> m r i }

instance IFunctor (ContT r m) where
  fmapI phi m = ContT (\b -> runContT m (b . phi))

instance IMonad (ContT r m) where
  returnI a = ContT $ \k -> k a
  bindI f m = ContT (\k -> runContT m (\x -> runContT (f x) k))

instance IMonadTrans (ContT r) where
  liftI m = contT (`bindI` m)

type Cont r a i = ContT r Identity a i

contT :: ((a :-> m r) -> m r i) -> ContT r m a i
contT = ContT

cont :: ((a :-> r) -> r i) -> Cont r a i
cont f = ContT (\c -> Identity (f (runIdentity . c)))

runCont :: Cont r a i -> (a :-> r) -> r i
runCont m k = runIdentity (runContT m (Identity . k))

evalContT :: IMonad m => ContT a m a i -> m a i
evalContT m = runContT m returnI

evalCont :: Cont a a i -> a i
evalCont m = runCont m id

mapContT :: (m r i -> m r j) -> ContT r m a i -> ContT r m a j
mapContT f m = contT $ f . runContT m

mapCont :: (r i -> r j) -> Cont r a i -> Cont r a j
mapCont f = mapContT (Identity . f . runIdentity)

callCCT  :: ((a :-> ContT r m x) -> ContT r m a i) -> ContT r m a i
callCCT f = contT $ \ c -> runContT (f (\ x -> contT $ \ _ -> c x)) c

callCC  :: ((a :-> Cont r x) -> Cont r a i) -> Cont r a i
callCC f = cont $ \ c -> runCont (f (\ x -> cont $ \ _ -> c x)) c

resetT :: (IMonad m, IMonadTrans t) => ContT a m a i -> t m a i
resetT = liftI . evalContT

shiftT :: IMonad m => ((a :-> m r) -> ContT r m r i) -> ContT r m a i
shiftT f = contT (evalContT . f)
