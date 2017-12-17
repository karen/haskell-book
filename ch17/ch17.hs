import Control.Applicative
import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data List a =
    Nil
  | Cons a (List a)
  deriving (Eq, Show)

instance Functor List where
    fmap _ Nil = Nil
    fmap f (Cons x xs) = Cons (f x) (fmap f xs)

-- (<*>) flst lst = flatMap (`fmap` lst) flst
-- For each function from flst
-- fmap over lst
-- then concat them together
instance Applicative List where
    pure x = Cons x Nil
    Nil <*> _ = Nil
    _ <*> Nil = Nil
    (<*>) flst lst = flatMap (\f -> fmap f lst) flst

instance (Arbitrary a) => Arbitrary (List a) where
    arbitrary = do
        a <- arbitrary
        elements [Nil, Cons a Nil]

instance Eq a => EqProp (List a) where
    (=-=) = eq

newtype ZipList' a =
    ZipList' (List a)
    deriving (Eq, Show)

instance Eq a => EqProp (ZipList' a) where
    xs =-= ys = xs' `eq` ys'
        where xs' = let (ZipList' l) = xs
                    in take' 3000 l
              ys' = let (ZipList' l) = ys
                    in take' 3000 l

instance Functor ZipList' where
     fmap f (ZipList' xs) = ZipList' $ fmap f xs

instance Applicative ZipList' where
    pure x = ZipList' (Cons x Nil)
    (<*>) (ZipList' flst) (ZipList' lst) = ZipList' (zip' flst lst)

instance (Arbitrary a) => Arbitrary (ZipList' a) where
    arbitrary = fmap ZipList' arbitrary

zip' :: List (t -> a) -> List t -> List a
zip' Nil _ = Nil
zip' _ Nil = Nil
zip' (Cons f fs) (Cons x xs) = Cons (f x) (zip' fs xs)

take' :: Int -> List a -> List a
take' 0 _ = Nil
take' _ Nil = Nil
take' n (Cons x xs) = Cons x (take' (n-1) xs)

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
newtype Identity a = Identity a deriving (Eq, Show)

instance Functor Identity where
    fmap f (Identity x) = Identity $ f x

instance Applicative Identity where
    pure x = Identity x
    (<*>) (Identity f) x = fmap f x

instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary = do
        x <- arbitrary
        return $ Identity x

instance Eq a => EqProp (Identity a) where
    (=-=) = eq

identityCheck = undefined :: (Identity (String, String, String))
--------------------------------------------------------------------------------
data Pair a = Pair a a deriving (Eq, Show)

instance Functor Pair where
    fmap f (Pair x x') = Pair (f x) (f x')

instance Applicative Pair where
    pure x = Pair x x
    (<*>) (Pair f g) (Pair x x') = Pair (f x) (g x')

instance Arbitrary a => Arbitrary (Pair a) where
    arbitrary = do
        x <- arbitrary
        y <- arbitrary
        return $ Pair x y

instance Eq a => EqProp (Pair a) where
    (=-=) = eq

pairCheck = undefined :: (Pair (String, String, String))
--------------------------------------------------------------------------------
data Two a b = Two a b deriving (Eq, Show)

instance Functor (Two a) where
    fmap f (Two x y) = Two x (f y)

instance (Monoid a) => Applicative (Two a) where
    pure x = Two mempty x
    (<*>) (Two f g) (Two x y) = Two (x `mappend` f) (g y)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
    arbitrary = do
        x <- arbitrary
        y <- arbitrary
        return $ Two x y

instance (Eq a, Eq b) => EqProp (Two a b) where
    (=-=) = eq

twoCheck = undefined :: (Two (String, String, String) (String, String, String))
--------------------------------------------------------------------------------
data Three' a b = Three' a b b deriving (Eq, Show)

instance Functor (Three' a) where
    fmap f (Three' x y y') = Three' x (f y) (f y')

instance (Monoid a) => Applicative (Three' a) where
    pure x = Three' mempty x x
    (<*>) (Three' a f g) (Three' a' x x') = Three' (a `mappend` a') (f x) (g x')

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
    arbitrary = do
        x <- arbitrary
        y <- arbitrary
        z <- arbitrary
        return $ Three' x y z

instance (Eq a, Eq b) => EqProp (Three' a b) where
    (=-=) = eq

threeCheck = undefined :: (Three' (String, String, String) (String, String, String))
--------------------------------------------------------------------------------
main :: IO ()
main = do
    quickBatch $ applicative (Cons (1:: Int, 2 :: Int, 3 :: Int) Nil)
    quickBatch $ applicative (ZipList' (Cons (1:: Int, 2 :: Int, 3 :: Int) Nil))
    quickBatch $ applicative identityCheck
    quickBatch $ applicative pairCheck
    quickBatch $ applicative twoCheck
    quickBatch $ applicative threeCheck
