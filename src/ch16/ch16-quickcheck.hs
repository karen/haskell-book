{-# LANGUAGE ViewPatterns #-}

import Test.QuickCheck
import Test.QuickCheck.Function

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Eq (f c), Functor f) => (a -> b) -> (b -> c) -> f a -> Bool
functorCompose f g x = (fmap g (fmap f x)) == (fmap (g . f) x)

functorCompose' :: (Eq (f c), Functor f) => f a -> Fun a b -> Fun b c -> Bool
functorCompose' x (Fun _ f) (Fun _ g) = (fmap (g . f) x) == (fmap g . fmap f $ x)

newtype Identity a = Identity a deriving (Eq, Show)
data Pair a = Pair a a deriving (Eq, Show)
data Two a b = Two a b deriving (Eq, Show)
data Three a b c = Three a b c deriving (Eq, Show)
data Four' a b = Four' a a a b deriving (Eq, Show)

instance Functor Identity where
    fmap f (Identity a) = Identity (f a)

instance (Arbitrary a) => Arbitrary (Identity a) where
    arbitrary = do
        x <- arbitrary
        return $ Identity x

type IdentityIdChk a = Identity a -> Bool
type IdentityCompose = Identity String -> Fun String Int -> Fun Int String -> Bool

instance Functor Pair where
    fmap f (Pair x x') = Pair (f x) (f x')

instance (Arbitrary a) => Arbitrary (Pair a) where
    arbitrary = do
        x <- arbitrary
        return $ Pair x x

type PairIdChk a = Pair a -> Bool
type PairCompose = Pair String -> Fun String Int -> Fun Int String -> Bool

instance Functor (Two a) where
    fmap f (Two x y) = Two x (f y)

instance Functor (Three a b) where
    fmap f (Three x y z) = Three x y (f z)

instance Functor (Four' a) where
    fmap f (Four' x x' x'' y) = Four' x x' x'' (f y)

data Wrap f a =
    Wrap (f a)
    deriving (Eq, Show)

instance (Functor f) => Functor (Wrap f) where
    fmap f (Wrap fa) = Wrap (f fa)

main :: IO ()
main = do
    quickCheck (functorIdentity :: IdentityIdChk String)
    quickCheck (functorCompose' :: IdentityCompose)
    quickCheck (functorIdentity :: PairIdChk String)
    quickCheck (functorCompose' :: PairCompose)
