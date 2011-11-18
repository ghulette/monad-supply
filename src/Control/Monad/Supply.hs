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

-- | Monad transformer.
newtype SupplyT s m a = SupplyT (StateT [s] m a)
    deriving (Functor, Monad, MonadTrans, MonadIO)
 
newtype Supply s a = Supply (SupplyT s Identity a)
    deriving (Functor, Monad, MonadSupply s)
 
instance Monad m => MonadSupply s (SupplyT s m) where
    supply = SupplyT $ do (x:xs) <- get
                          put xs
                          return x

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
