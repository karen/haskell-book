import Data.Semigroup
import Test.QuickCheck(quickCheck,
                       Arbitrary,
                       arbitrary,
                       frequency,
                       elements)

data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
    _ <> _ = Trivial

instance Arbitrary Trivial where
    arbitrary = return Trivial

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

type TrivialAssoc = Trivial -> Trivial -> Trivial -> Bool
--------------------------------------------------------------------------------
newtype Identity a = Identity a deriving (Eq, Show)

instance Semigroup (Identity a) where
    x <> _ = x

instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary = do
        x <- arbitrary
        return (Identity x)

type IdentityAssoc a = Identity a -> Identity a -> Identity a -> Bool
--------------------------------------------------------------------------------
data Two a b = Two a b deriving (Eq, Show)

instance Semigroup (Two a b) where
    (Two x y) <> (Two x' y') = (Two x y')

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
    arbitrary = do
        x <- arbitrary
        y <- arbitrary
        return $ Two x y

type TwoAssoc a b = Two a b -> Two a b -> Two a b -> Bool

data Three a b c = Three a b c deriving (Eq, Show)

instance Semigroup (Three a b c) where
    (Three x y z) <> (Three x' y' z') = (Three x y' z)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
    arbitrary = do
        x <- arbitrary
        y <- arbitrary
        z <- arbitrary
        return $ Three x y z

type ThreeAssoc a b c = Three a b c -> Three a b c -> Three a b c -> Bool

newtype BoolConj = BoolConj Bool deriving (Eq, Show)

instance Semigroup BoolConj where
    (BoolConj x) <> (BoolConj y) = BoolConj (x && y)

instance Arbitrary BoolConj where
    arbitrary = elements [BoolConj False, BoolConj True]

type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool

newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)

instance Semigroup BoolDisj where
    (BoolDisj x) <> (BoolDisj y) = BoolDisj (x || y)

instance Arbitrary BoolDisj where
    arbitrary = elements [BoolDisj False, BoolDisj True]

type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool

data Or a b = Fst a | Snd b deriving (Eq, Show)

instance Semigroup (Or a b) where
    (Snd x) <> _ = Snd x
    _ <> (Snd x) = Snd x
    _ <> x = x

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
    arbitrary = do
        x <- arbitrary
        y <- arbitrary
        frequency [(1, return $ Fst x),
                   (1, return $ Snd y)]

type OrAssoc a b = Or a b -> Or a b -> Or a b -> Bool

newtype Combine a b = Combine { unCombine :: (a -> b) }

instance (Semigroup b) => Semigroup (Combine a b) where
    Combine f <> Combine g = Combine (f <> g)

newtype Comp a = Comp { unComp :: (a -> a) }

instance Semigroup a => Semigroup (Comp a) where
    Comp f <> Comp g = Comp $ f <> g

data Validation a b =
    Failure a | Success b
    deriving (Eq, Show)

instance Semigroup a => Semigroup (Validation a b) where
    Success x <> _ = Success x
    _ <> Success x = Success x
    Failure y <> Failure y' = Failure $ y <> y'

instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation a b) where
    arbitrary = do
        x <- arbitrary
        y <- arbitrary
        frequency [(1, return $ Success x),
                   (1, return $ Failure y)]

type ValidationAssoc a b = Validation a b -> Validation a b -> Validation a b -> Bool

newtype AccumulateRight a b =
    AccumulateRight (Validation a b)
    deriving (Eq, Show)

instance Semigroup b => Semigroup (AccumulateRight a b) where
    AccumulateRight (Success x) <> AccumulateRight (Success x') = AccumulateRight $ Success $ x <> x'
    _ <> AccumulateRight (Success x) = AccumulateRight $ Success x
    AccumulateRight (Success x) <> _ = AccumulateRight $ Success x
    AccumulateRight (Failure x) <> AccumulateRight (Failure x') = AccumulateRight $ Failure x'

instance (Arbitrary a, Arbitrary b) => Arbitrary (AccumulateRight a b) where
    arbitrary = do
        x <- arbitrary
        y <- arbitrary
        frequency [(1, return $ AccumulateRight $ Success x),
                   (1, return $ AccumulateRight $ Success y)]

type AccumulateRightAssoc a b = AccumulateRight a b -> AccumulateRight a b -> AccumulateRight a b -> Bool

newtype AccumulateBoth a b =
    AccumulateBoth (Validation a b)
    deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (AccumulateBoth a b) where
    AccumulateBoth (Success x) <> AccumulateBoth (Success x') = AccumulateBoth $ Success $ x <> x'
    AccumulateBoth (Failure x) <> AccumulateBoth (Failure x') = AccumulateBoth $ Failure $ x <> x'
    _ <> AccumulateBoth (Failure x') = AccumulateBoth $ Failure x'
    AccumulateBoth (Failure x) <> _ = AccumulateBoth $ Failure x

instance (Arbitrary a, Arbitrary b) => Arbitrary (AccumulateBoth a b) where
    arbitrary = do
        x <- arbitrary
        y <- arbitrary
        frequency [(1, return $ AccumulateBoth $ Success x),
                   (1, return $ AccumulateBoth $ Success y)]

type AccumulateBothAssoc a b = AccumulateBoth a b -> AccumulateBoth a b -> AccumulateBoth a b -> Bool

main :: IO ()
main = quickCheck (semigroupAssoc :: AccumulateBothAssoc String String)
