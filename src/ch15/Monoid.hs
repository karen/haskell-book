import Data.Semigroup
import Test.QuickCheck(quickCheck,
                       Arbitrary,
                       arbitrary,
                       frequency,
                       elements)

-- Tests
semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty `mappend` a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a `mappend` mempty) == a
--------------------------------------------------------------------------------
data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
    _ <> _ = Trivial

instance Monoid Trivial where
    mempty = Trivial
    mappend = (<>)

instance Arbitrary Trivial where
    arbitrary = return Trivial

type TrivialAssoc = Trivial -> Trivial -> Trivial -> Bool
--------------------------------------------------------------------------------
newtype Identity a = Identity a deriving (Eq, Show)

instance (Semigroup a) => Semigroup (Identity a) where
    Identity x <> Identity x' = Identity (x <> x')

instance (Semigroup a, Monoid a) => Monoid (Identity a) where
    mempty = Identity mempty
    mappend = (<>)

instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary = do
        x <- arbitrary
        return (Identity x)

type IdentityAssoc a = Identity a -> Identity a -> Identity a -> Bool
--------------------------------------------------------------------------------
data Two a b = Two a b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
    (Two x y) <> (Two x' y') = Two (x <> x') (y <> y')

instance (Semigroup a, Semigroup b, Monoid a, Monoid b) => Monoid (Two a b) where
    mempty = Two mempty mempty
    mappend = (<>)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
    arbitrary = do
        x <- arbitrary
        y <- arbitrary
        return $ Two x y

type TwoAssoc a b = Two a b -> Two a b -> Two a b -> Bool
--------------------------------------------------------------------------------
newtype BoolConj = BoolConj Bool deriving (Eq, Show)

instance Semigroup BoolConj where
    (BoolConj x) <> (BoolConj y) = BoolConj (x && y)

instance Monoid BoolConj where
    mempty = BoolConj True
    mappend = (<>)

instance Arbitrary BoolConj where
    arbitrary = elements [BoolConj False, BoolConj True]

type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool
--------------------------------------------------------------------------------
newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)

instance Semigroup BoolDisj where
    (BoolDisj x) <> (BoolDisj y) = BoolDisj (x || y)

instance Monoid BoolDisj where
    mempty = BoolDisj False
    mappend = (<>)

instance Arbitrary BoolDisj where
    arbitrary = elements [BoolDisj False, BoolDisj True]

type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool
--------------------------------------------------------------------------------
newtype Combine a b = Combine { unCombine :: (a -> b) }

instance (Semigroup b) => Semigroup (Combine a b) where
    Combine f <> Combine g = Combine (f <> g)

instance (Semigroup b, Monoid a, Monoid b) => Monoid (Combine a b) where
    mempty = Combine (\x -> mempty)
    mappend = (<>)
--------------------------------------------------------------------------------
newtype Comp a = Comp { unComp :: (a -> a) }

instance Semigroup a => Semigroup (Comp a) where
    Comp f <> Comp g = Comp $ f <> g

instance (Semigroup a, Monoid a) => Monoid (Comp a) where
    mempty = Comp $ \x -> mempty
    mappend = (<>)
--------------------------------------------------------------------------------
main :: IO ()
main = do
    quickCheck (semigroupAssoc :: TrivialAssoc)
    quickCheck (semigroupAssoc :: IdentityAssoc String)
    quickCheck (semigroupAssoc :: TwoAssoc String String)
    quickCheck (semigroupAssoc :: BoolConjAssoc)
    quickCheck (semigroupAssoc :: BoolDisjAssoc)
    quickCheck (monoidLeftIdentity :: Trivial -> Bool)
    quickCheck (monoidRightIdentity :: Trivial -> Bool)
    quickCheck (monoidLeftIdentity :: Identity String -> Bool)
    quickCheck (monoidRightIdentity :: Identity String -> Bool)
    quickCheck (monoidLeftIdentity :: Two String String -> Bool)
    quickCheck (monoidRightIdentity :: Two String String -> Bool)
    quickCheck (monoidLeftIdentity :: BoolConj -> Bool)
    quickCheck (monoidLeftIdentity :: BoolDisj -> Bool)
    quickCheck (monoidRightIdentity :: BoolConj -> Bool)
