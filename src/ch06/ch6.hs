
data TisAnInteger =
    TisAn Integer

instance Eq TisAnInteger where
    (==) (TisAn a)  (TisAn b) = a == b

data TwoIntegers =
    Two Integer Integer

instance Eq TwoIntegers where
    (==) (Two a b) (Two a' b') = a == a' && b == b'

data StringOrInt =
    TisAnInt Int
  | TisAString String

instance Eq StringOrInt where
    (==) (TisAnInt x) (TisAnInt x') = x == x'
    (==) (TisAString x) (TisAString x') = x == x'
    (==) _ _ = False

data Pair a =
    Pair a a

instance Eq a => Eq (Pair a) where
    (==) (Pair a b) (Pair a' b') = a == a' && b == b'

data Tuple a b =
    Tuple a b

instance (Eq a, Eq b) => Eq (Tuple a b) where
    (==) (Tuple a b) (Tuple a' b') = a == a' && b == b'

data Which a =
    ThisOne a
  | ThatOne a

instance (Eq a) => Eq (Which a) where
    (==) (ThisOne a) (ThisOne a') = a == a'
    (==) (ThatOne a) (ThatOne a') = a == a'
    (==) _ _ = False

data EitherOr a b =
    Hello a
  | Goodbye b

instance (Eq a, Eq b) => Eq (EitherOr a b) where
    (==) (Hello a) (Hello a') = a == a'
    (==) (Goodbye a) (Goodbye a') = a == a'
    (==) _ _ = False

data Person = Person Bool
    deriving Show
printPerson :: Person -> IO ()
printPerson person = putStrLn (show person)

data Mood = Blah
          | Woot deriving (Show, Eq)

settleDown :: Mood -> Mood
settleDown x = if x == Woot then Blah else x

data Rocks = Rocks String deriving (Eq, Show)
data Yeah = Yeah Bool deriving (Eq, Show)
data Papu = Papu Rocks Yeah deriving (Eq, Show)

chk :: Eq b => (a -> b) -> a -> b -> Bool
chk f a b = (f a) == b

arith:: Num b => (a -> b) -> Integer -> a -> b
arith f i a = f a + fromIntegral i
