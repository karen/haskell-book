{-# LANGUAGE InstanceSigs #-}

newtype EitherT e m a =
    EitherT { runEitherT :: m (Either e a) }

instance Functor m => Functor (EitherT e m) where
    fmap f (EitherT mea) = EitherT $ (fmap . fmap) f mea

instance Applicative m => Applicative (EitherT e m) where
    pure x = EitherT $ pure (Right x)

    (<*>) :: EitherT e m (a -> b)
          -> EitherT e m a
          -> EitherT e m b
    (EitherT mab) <*> (EitherT mea) = EitherT $ (<*>) <$> mab <*> mea

instance Monad m => Monad (EitherT e m) where
    return = pure

    (>>=) :: EitherT e m a
          -> (a -> EitherT e m b)
          -> EitherT e m b
    (EitherT mea) >>= aemb = EitherT $ do
        ea <- mea
        case ea of
            Left e -> pure (Left e)
            Right a -> (runEitherT . aemb) a

swapEitherT :: (Functor m) => EitherT e m a -> EitherT a m e
swapEitherT (EitherT mea) = EitherT $ swapEither <$> mea

swapEither :: Either e a -> Either a e
swapEither (Left e) = Right e
swapEither (Right a) = Left a

eitherT :: Monad m => (a -> m c) -> (b -> m c) -> (EitherT a m b) -> m c
eitherT amc bmc (EitherT mab) = mab >>= either amc bmc