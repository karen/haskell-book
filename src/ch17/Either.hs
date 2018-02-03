import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Sum' a b =
    First a
  | Second b
  deriving (Eq, Show)

data Validation e a =
    Error' e
  | Success' a
  deriving (Eq, Show)

instance Functor (Sum' a) where
    fmap _ (First x) = First x
    fmap f (Second a) = Second $ f a

instance Applicative (Sum' a) where
    pure x = Second x
    First a <*> _ = First a
    _ <*> First a = First a
    Second b <*> Second b' = Second $ b b'

instance (Arbitrary a, Arbitrary b) => Arbitrary (Sum' a b) where
    arbitrary = do
        x <- arbitrary
        y <- arbitrary
        elements [First x, Second y]

instance (Eq a, Eq b) => EqProp (Sum' a b) where
    (=-=) = eq

instance Functor (Validation e) where
    fmap _ (Error' e) = Error' e
    fmap f (Success' a) = Success' $ f a

instance Monoid e => Applicative (Validation e) where
    pure x = Success' x
    Error' e <*> Error' e' = Error' $ e `mappend` e'
    Error' e <*> _ = Error' e
    _ <*> Error' e = Error' e
    Success' x <*> Success' x' = Success' $ x x'

instance (Arbitrary e, Arbitrary a) => Arbitrary (Validation e a) where
    arbitrary = do
        x <- arbitrary
        y <- arbitrary
        elements [Success' x, Error' y]

instance (Eq e, Eq a) => EqProp (Validation e a) where
    (=-=) = eq

sumCheck = undefined :: Sum' (Int, Int, Int) (Int, Int, Int)
valCheck = undefined :: Validation (String, String, String) (String, String, String)

main :: IO ()
main = do
    quickBatch $ applicative $ sumCheck
    quickBatch $ applicative $ valCheck
