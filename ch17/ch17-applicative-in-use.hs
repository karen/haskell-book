import Control.Applicative
import Data.List (elemIndex)
import Data.Monoid

f x = lookup x [(3, "hello"), (4, "julie"), (5, "kbai")]
g y = lookup y [(7, "sup?"), (8, "chris"), (9, "aloha")]

h z = lookup z [(2,3),(5,6),(7,8)]
m x = lookup x [(4, 10), (8, 13), (1, 9001)]

added :: Maybe Integer
added = (+3) <$> (lookup 3 $ zip [1,2,3] [4,5,6])

y :: Maybe Integer
y = lookup 3 $ zip [1, 2, 3] [4, 5, 6]

z :: Maybe Integer
z = lookup 2 $ zip [1, 2, 3] [4, 5, 6]

tupled :: Maybe (Integer, Integer)
tupled = liftA2 (,) y z

x :: Maybe Int
x = elemIndex 3 [1..5]

y' :: Maybe Int
y' = elemIndex 4 [1..5]

max' :: Int -> Int -> Int
max' = max

-- maxed = (max') <$> x <*> y'
maxed :: Maybe Int
maxed = liftA2 max' x y'

xs = [1,2,3]
ys = [4,5,6]

j :: Maybe Integer
j = lookup 3 $ zip xs ys

k :: Maybe Integer
k = lookup 2 $ zip xs ys

summed :: Maybe Integer
summed = sum <$> (liftA2 (,) j k)

newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
    fmap f (Identity x) = Identity (f x)

instance Applicative Identity where
    pure x = Identity x
    (<*>) (Identity g) (Identity x) = Identity (g x)

newtype Constant a b = Constant { getConstant :: a } deriving (Eq, Ord, Show)

instance Functor (Constant a) where
    fmap _ (Constant a) = Constant a

instance Monoid a => Applicative (Constant a) where
    pure _ = Constant mempty
    (<*>) (Constant a) (Constant a') = Constant (a `mappend` a')
