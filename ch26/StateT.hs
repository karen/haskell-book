{-# LANGUAGE InstanceSigs #-}

import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader

newtype StateT s m a =
    StateT { runStateT :: s -> m (a,s) }

instance (Functor m) => Functor (StateT s m) where
    fmap f (StateT smas) = StateT $ \s -> let mas = smas s
                                          in fmap (\(a,s') -> (f a, s')) mas

instance (Monad m) => Applicative (StateT s m) where
    pure a = StateT $ \s -> pure (a,s)

    -- Same thing but without do syntax
    -- (StateT smab) <*> (StateT sma) = StateT $ \s -> let mf = smab s
    --                                                 in join $ fmap (\(ab, s') ->
    --                                                         fmap (\(a, s'') -> (ab a, s''))
    --                                                         (sma s')) mf
    (<*>) :: StateT s m (a -> b)
          -> StateT s m a
          -> StateT s m b
    (StateT smab) <*> (StateT sma) = StateT $ \s -> do
        (ab, s') <- smab s
        (a, s'') <- sma s'
        return (ab a, s'')

instance (Monad m) => Monad (StateT s m) where
    return = pure

    (>>=) :: StateT s m a
          -> (a -> StateT s m b)
          -> StateT s m b
    (StateT sma) >>= f = StateT $ \s -> do
        (a, s') <- sma s
        (runStateT . f) a s'
