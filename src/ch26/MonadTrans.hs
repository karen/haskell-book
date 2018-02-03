{-# LANGUAGE InstanceSigs #-}

import Control.Monad (liftM)
import Control.Monad.IO.Class
import Control.Monad.Trans.Class

newtype MaybeT m a =
    MaybeT { runMaybeT :: m (Maybe a) }

newtype EitherT e m a =
    EitherT { runEitherT :: m (Either e a) }

newtype ReaderT r m a =
    ReaderT { runReaderT :: r -> m a }

newtype StateT s m a =
    StateT { runStateT :: s -> m (a,s) }

instance MonadTrans MaybeT where
    lift :: Monad m => m a -> MaybeT m a
    lift = MaybeT . liftM Just

instance MonadTrans (ReaderT r) where
    lift :: Monad m => m a -> ReaderT r m a
    lift = ReaderT . const

instance MonadTrans (EitherT e) where
    lift :: Monad m => m a -> EitherT e m a
    lift = EitherT . liftM pure

instance MonadTrans (StateT s) where
    lift :: Monad m => m a -> StateT s m a
    lift ma = StateT (\s -> do
                a <- ma
                return (a, s))

instance (MonadIO m) => MonadIO (EitherT e m) where
    liftIO :: IO a -> EitherT e m a
    liftIO = lift . liftIO

instance (MonadIO m) => MonadIO (MaybeT m) where
    liftIO :: IO a -> MaybeT m a
    liftIO = lift . liftIO

instance (MonadIO m) => MonadIO (ReaderT r m) where
    liftIO :: IO a -> ReaderT r m a
    liftIO = lift . liftIO

instance (MonadIO m) => MonadIO (StateT s m) where
    liftIO :: IO a -> StateT s m a
    liftIO = lift . liftIO
