import Data.Monoid
import Test.QuickCheck

data Optional a =
    Nada
  | Only a
  deriving (Eq, Show)

instance Monoid a => Monoid (Optional a) where
    mempty = Nada
    mappend Nada sth = sth
    mappend sth Nada = sth
    mappend (Only a) (Only a') = Only (a `mappend` a')

newtype First' a =
    First' { getFirst' :: Optional a }
    deriving (Eq, Show)

instance Monoid (First' a) where
    mempty = First' Nada
    mappend (First' Nada) x = x
    mappend x _ = x 

firstMappend :: First' a -> First' a -> First' a
firstMappend = mappend

type FirstMappend =
     First' String
  -> First' String
  -> First' String
  -> Bool

instance Arbitrary a => Arbitrary (First' a) where
    arbitrary = do
        x <- arbitrary
        frequency [ (1, return $ First' Nada),
                    (3, return $ First' (Only x)) ]

monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

main :: IO ()
main = do
    quickCheck (monoidAssoc :: FirstMappend)
    quickCheck (monoidLeftIdentity :: First' String -> Bool)
    quickCheck (monoidRightIdentity :: First' String -> Bool)

