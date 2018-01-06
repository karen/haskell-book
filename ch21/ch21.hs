{-# LANGUAGE FlexibleInstances #-}
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Query = Query
data SomeObj = SomeObj
data IoOnlyObj = IoOnlyObj
data Err = Err

decodeFn :: String -> Either Err SomeObj
decodeFn = undefined

fetchFn :: Query -> IO [String]
fetchFn = undefined

makeIoOnlyObj :: [SomeObj] -> IO [(SomeObj, IoOnlyObj)]
makeIoOnlyObj = undefined
--------------------------------------------------------------------------------
newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
    fmap f (Identity a) = Identity (f a)

instance Foldable Identity where
    foldr f z (Identity a) = f a z

instance Traversable Identity where
    traverse f (Identity a) = fmap Identity (f a)

instance (Arbitrary a) => Arbitrary (Identity a) where
    arbitrary = do
        x <- arbitrary
        return (Identity x)

instance (Eq a) => EqProp (Identity a) where
    (=-=) = eq
--------------------------------------------------------------------------------
newtype Constant a b = Constant { getConstant :: a } deriving (Eq, Ord, Show)

instance Functor (Constant a) where
    fmap _ (Constant a) = (Constant a)

instance Foldable (Constant a) where
    foldr _ z _ = z

instance Traversable (Constant a) where
    traverse f (Constant a) = fmap Constant (pure a)

instance (Arbitrary a) => Arbitrary (Constant a b) where
    arbitrary = do
        x <- arbitrary
        return (Constant x)

instance (Eq a) => EqProp (Constant a b) where
    (=-=) = eq
--------------------------------------------------------------------------------
data Optional a = Nada | Yep a deriving (Eq, Ord, Show)

instance Functor Optional where
    fmap _ Nada = Nada
    fmap f (Yep a) = Yep (f a)

instance Foldable Optional where
    foldr _ z Nada = z
    foldr f z (Yep a) = f a z

instance Traversable Optional where
    traverse f Nada = pure Nada
    traverse f (Yep a) = fmap Yep (f a)

instance (Arbitrary a) => Arbitrary (Optional a) where
    arbitrary = do
        x <- arbitrary
        elements [Nada, Yep x]

instance (Eq a) => EqProp (Optional a) where
    (=-=) = eq
--------------------------------------------------------------------------------
data List a = Nil | Cons a (List a) deriving (Eq, Ord, Show)

instance Functor List where
    fmap _ Nil = Nil
    fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Foldable List where
    foldr _ z Nil = z
    foldr f z (Cons x xs) = f x (foldr f z xs)

instance Traversable List where
    traverse f Nil = pure Nil
    traverse f (Cons x xs) = Cons <$> (f x) <*> (traverse f xs)

instance (Arbitrary a) => Arbitrary (List a) where
    arbitrary = do
        a <- arbitrary
        elements [Nil, Cons a Nil]

instance Eq a => EqProp (List a) where
    (=-=) = eq
--------------------------------------------------------------------------------
data Three a b c = Three a b c deriving (Eq, Ord, Show)

instance Functor (Three a b) where
    fmap f (Three a b c) = Three a b (f c)

instance Foldable (Three a b) where
    foldr f z (Three a b c) = f c z

instance Traversable (Three a b) where
    traverse f (Three a b c) = fmap (Three a b) (f c)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
    arbitrary = do
        x <- arbitrary
        y <- arbitrary
        z <- arbitrary
        return $ Three x y z

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
    (=-=) = eq
--------------------------------------------------------------------------------
data Three' a b = Three' a b b deriving (Eq, Ord, Show)

instance Functor (Three' a) where
    fmap f (Three' x y y') = Three' x (f y) (f y')

instance Foldable (Three' a) where
    foldr f z (Three' x y y') = f y $ f y' z

instance Traversable (Three' a) where
    traverse f (Three' x y y') = (Three' x) <$> (f y) <*> (f y')

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
    arbitrary = do
        x <- arbitrary
        y <- arbitrary
        z <- arbitrary
        return $ Three' x y z

instance (Eq a, Eq b) => EqProp (Three' a b) where
    (=-=) = eq
--------------------------------------------------------------------------------
data S f a = S (f a) a deriving (Eq, Ord, Show)

instance Functor f => Functor (S f) where
    fmap g (S t a) = S (fmap g t) (g a)

instance Foldable n => Foldable (S n) where
    foldr f z (S t a) = foldr f (f a z) t

instance Traversable n => Traversable (S n) where
    traverse f (S t a) = S <$> traverse f t <*> f a

instance Arbitrary a => Arbitrary (S Maybe a) where
  arbitrary = do
    a <- arbitrary
    a' <- arbitrary
    oneof [ return $ S Nothing a
          , return $ S (Just a') a ]

instance Eq a => EqProp (S Maybe a) where (=-=) = eq
--------------------------------------------------------------------------------
main = do
    let identityChk = undefined :: Identity (Int, Int, [Int])
    let constantChk = undefined :: Constant (Int, Int, [Int]) (Int, Int, [Int])
    let optionalChk = undefined :: Optional (Int, Int, [Int])
    let listChk = undefined :: List (Int, Int, [Int])
    let threeChk = undefined :: Three (Int, Int, [Int]) (Int, Int, [Int]) (Int, Int, [Int])
    let threeChk' = undefined :: Three' (Int, Int, [Int]) (Int, Int, [Int])
    let sChk = undefined :: S Maybe (Int, Int, [Int])
    quickBatch (traversable identityChk)
    quickBatch (traversable constantChk)
    quickBatch (traversable optionalChk)
    quickBatch (traversable listChk)
    quickBatch (traversable threeChk)
    quickBatch (traversable threeChk')
    quickBatch (traversable sChk)
