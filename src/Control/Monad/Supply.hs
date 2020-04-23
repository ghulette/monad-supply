{-# LANGUAGE CPP                        #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances       #-}

-- | Support for computations which consume values from a (possibly infinite)
-- supply. See <http://www.haskell.org/haskellwiki/New_monads/MonadSupply> for
-- details.

module Control.Monad.Supply
( MonadSupply (..)
, SupplyT
, Supply
, evalSupplyT
, evalSupply
, runSupplyT
, runSupply
, supplies
) where

import           Control.Monad.Except
#if !MIN_VERSION_base(4,11,0)
import           Control.Monad.Fail
#endif
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Writer   hiding ((<>))
#if !MIN_VERSION_base(4,9,0)
import           Data.Semigroup
#endif
import qualified Control.Monad.Trans.State.Lazy as LazyState

class Monad m => MonadSupply s m | m -> s where
  supply :: m s
  peek :: m s
  exhausted :: m Bool

-- | Supply monad transformer.
newtype SupplyT s m a = SupplyT { unSupplyT :: StateT [s] m a }
  deriving (Functor, Applicative, Monad, MonadTrans, MonadIO, MonadFix)

instance MonadError e m => MonadError e (SupplyT s m) where
    throwError = lift . throwError
    catchError m h = SupplyT $ LazyState.liftCatch catchError (unSupplyT m) (unSupplyT . h)

-- | Supply monad.
newtype Supply s a = Supply (SupplyT s Maybe a)
  deriving (Functor, Applicative, Monad, MonadSupply s, MonadFix)

instance MonadFail m => MonadSupply s (SupplyT s m) where
  supply = SupplyT $ do (x:xs) <- get
                        put xs
                        return x
  peek = SupplyT $ gets head
  exhausted = SupplyT $ gets null

-- Monad transformer instances
instance (MonadSupply s m) => MonadSupply s (ExceptT e m) where
  supply = lift supply
  peek = lift peek
  exhausted = lift exhausted

instance MonadSupply s m => MonadSupply s (StateT st m) where
  supply = lift supply
  peek = lift peek
  exhausted = lift exhausted

instance MonadSupply s m => MonadSupply s (ReaderT r m) where
  supply = lift supply
  peek = lift peek
  exhausted = lift exhausted

instance (Monoid w, MonadSupply s m) => MonadSupply s (WriterT w m) where
  supply = lift supply
  peek = lift peek
  exhausted = lift exhausted

instance Semigroup a => Semigroup (Supply s a) where
  m1 <> m2 = (<>) <$> m1 <*> m2

instance (Semigroup a, Monoid a) => Monoid (Supply s a) where
  mempty = return mempty
  m1 `mappend` m2 = m1 <> m2

-- | Get n supplies.
supplies :: MonadSupply s m => Int -> m [s]
supplies n = replicateM n supply

evalSupplyT :: Monad m => SupplyT s m a -> [s] -> m a
evalSupplyT (SupplyT s) = evalStateT s

evalSupply :: Supply s a -> [s] -> Maybe a
evalSupply (Supply s) = evalSupplyT s

runSupplyT :: Monad m => SupplyT s m a -> [s] -> m (a,[s])
runSupplyT (SupplyT s) = runStateT s

runSupply :: Supply s a -> [s] -> Maybe (a,[s])
runSupply (Supply s) = runSupplyT s
