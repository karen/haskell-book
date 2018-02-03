import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Nope a = NopedotJpg deriving (Eq, Show)

instance Functor Nope where
    fmap _ _ = NopedotJpg

instance Applicative Nope where
    pure _ = NopedotJpg
    _ <*> _ = NopedotJpg

instance Monad Nope where
    return = pure
    _ >>= _ = NopedotJpg

instance Arbitrary (Nope a) where
    arbitrary = do
        return NopedotJpg

instance EqProp (Nope a) where
    (=-=) = eq
--------------------------------------------------------------------------------
data PhbtEither b a = Left' a | Right' b deriving (Eq, Show)

instance Functor (PhbtEither b) where
    fmap f (Left' a)  = Left' (f a)
    fmap _ (Right' b) = Right' b

instance Applicative (PhbtEither b) where
    pure x         = Left' x
    Left' f <*> x  = fmap f x
    Right' f <*> _ = Right' f

instance Monad (PhbtEither b) where
    return = pure
    Left' x >>= f = f x
    Right' x >>= _ = Right' x

instance (Arbitrary a, Arbitrary b) => Arbitrary (PhbtEither b a) where
    arbitrary = do
        x <- arbitrary
        y <- arbitrary
        elements [Left' x, Right' y]

instance (Eq a, Eq b) => EqProp (PhbtEither b a) where
    (=-=) = eq
--------------------------------------------------------------------------------
newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
    fmap f (Identity x) = Identity $ f x

instance Applicative Identity where
    pure x = Identity x
    Identity f <*> x = fmap f x

instance Monad Identity where
    return = pure
    Identity x >>= f = f x

instance (Arbitrary a) => Arbitrary (Identity a) where
    arbitrary = do
        x <- arbitrary
        return (Identity x)

instance (Eq a) => EqProp (Identity a) where
    (=-=) = eq
--------------------------------------------------------------------------------
data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Functor List where
    fmap _ Nil = Nil
    fmap f (Cons x xs) = Cons (f x) (fmap f xs)

-- Cons f fs <*> xs = (fmap f xs) <> (fs <*> xs) 
instance Applicative List where
    pure x = Cons x Nil
    Nil <*> _ = Nil
    _ <*> Nil = Nil
    (<*>) flst lst = flatMap (\f -> fmap f lst) flst

-- Cons x xs >>= f = (f x) <> (xs >>= f)
instance Monad List where
    return = pure
    Nil >>= f = Nil
    Cons x xs >>= f = append (f x) (xs >>= f)

instance (Arbitrary a) => Arbitrary (List a) where
    arbitrary = do
        a <- arbitrary
        elements [Nil, Cons a Nil]

instance Eq a => EqProp (List a) where
    (=-=) = eq

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x $ xs `append` ys

fold :: (a->b->b) -> b -> List a -> b
fold _ b Nil        = b
fold f b (Cons h t) = f h (fold f b t)

concat' :: List (List a) -> List a
concat' = fold append Nil

flatMap :: (a -> List b) -> List a -> List b
flatMap f as = concat' $ fmap f as
--------------------------------------------------------------------------------
j :: Monad m => m (m a) -> m a
j = (>>= id)

l1 :: Monad m => (a -> b) -> m a -> m b
l1 f x = fmap f x

l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 f ma mb = ma >>=
    \x -> (mb >>=
        (\y -> return $ f x y))

l2' :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2' f ma mb = do
    x <- ma
    y <- mb
    return (f x y)

a :: Monad m => m a -> m (a -> b) -> m b
a ma f = do
    f' <- f
    a <- ma
    return $ f' a

a' :: Monad m => m a -> m (a -> b) -> m b
a' ma f = f >>=
    \f' -> (ma >>=
        (\a -> return $ f' a))

meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh (x:[]) f = f x >>= (\x' -> return [x'])
meh (x:xs) f = f x >>= (\x -> fmap (x:) (meh xs f))

flipType :: (Monad m) => [m a] -> m [a]
flipType mas = meh mas id

main = do
    let nopeCheck = undefined :: Nope (String, String, String)
    quickBatch $ functor nopeCheck
    quickBatch $ applicative nopeCheck
    quickBatch $ monad nopeCheck
    let eitherCheck = undefined :: PhbtEither (String, String, String) (String, String, String)
    quickBatch $ functor eitherCheck
    quickBatch $ applicative eitherCheck
    quickBatch $ monad eitherCheck
    let identityCheck = undefined :: Identity (String, String, String)
    quickBatch $ functor identityCheck
    quickBatch $ applicative identityCheck
    quickBatch $ monad identityCheck
    let listCheck = undefined :: List (String, String, String)
    quickBatch $ monad listCheck