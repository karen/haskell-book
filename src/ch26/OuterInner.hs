module OuterInner where

embedded :: MaybeT (ExceptT String (ReaderT () IO)) Int
embedded = return 1

embedded' :: MaybeT (ExceptT String (ReaderT () IO)) Int
embedded' =  MaybeT $ ExceptT $ ReaderT $ const(return (Right (Just 1)))
