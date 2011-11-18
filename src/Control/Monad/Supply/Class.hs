{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Control.Monad.Supply.Class
( MonadSupply
, supply
) where

class Monad m => MonadSupply s m | m -> s where
  supply :: m s