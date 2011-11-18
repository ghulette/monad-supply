{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Control.Monad.Supply
( module Control.Monad.Supply.Class
, SupplyT
, Supply
, evalSupplyT
, evalSupply
, runSupplyT
, runSupply
, supplies
) where

import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Supply.Class
import Data.Monoid

-- | Monad transformer.
newtype SupplyT s m a = SupplyT (StateT [s] m a)
    deriving (Functor, Monad, MonadTrans, MonadIO)
 
newtype Supply s a = Supply (SupplyT s Identity a)
    deriving (Functor, Monad, MonadSupply s)
 
instance Monad m => MonadSupply s (SupplyT s m) where
    supply = SupplyT $ do (x:xs) <- get
                          put xs
                          return x

-- Actually any monad/monoid pair gives rise to a new monoid, i.e.
--   instance (Monad m,Monoid a) => Monoid (m a)
-- but we can't write it like that because it conflicts with existing
-- instances provided by Data.Monoid.
instance (Monoid a) => Monoid (Supply s a) where
  mempty = return mempty
  m1 `mappend` m2 = do
    x1 <- m1
    x2 <- m2
    return (x1 `mappend` x2)

supplies :: MonadSupply s m => Int -> m [s]
supplies n = replicateM n supply

evalSupplyT :: Monad m => SupplyT s m a -> [s] -> m a
evalSupplyT (SupplyT s) = evalStateT s

evalSupply :: Supply s a -> [s] -> a
evalSupply (Supply s) = runIdentity . evalSupplyT s

runSupplyT :: Monad m => SupplyT s m a -> [s] -> m (a,[s])
runSupplyT (SupplyT s) = runStateT s

runSupply :: Supply s a -> [s] -> (a,[s])
runSupply (Supply s) = runIdentity . runSupplyT s
