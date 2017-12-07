{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

data Price = Price Integer deriving (Eq, Show)
data Manufacturer = Mini | Mazda | Tata deriving (Eq, Show)
data Airline = PapuAir | CatapultsR'Us | TakeYourChancesUnited deriving (Eq, Show)
data Vehicle = Car Manufacturer Price | Plane Airline deriving (Eq, Show)

myCar = Car Mini (Price 14000)
urCar = Car Mazda (Price 20000)
clownCar = Car Tata (Price 7000)
doge = Plane PapuAir

isCar :: Vehicle -> Bool
isCar (Plane _) = False
isCar (Car _ _) = True

isPlane :: Vehicle -> Bool
isPlane (Plane _) = True
isPlane (Car _ _) = False

areCars :: [Vehicle] -> [Bool]
areCars = map isCar

class TooMany a where
    tooMany :: a -> Bool

instance TooMany Int where
    tooMany n = n > 42

-- newtype Goats = Goats Int deriving (Eq, Show)
-- instance TooMany Goats where
--    tooMany (Goats n) = tooMany n

newtype Goats = Goats Int deriving (Eq, Show, TooMany)

instance TooMany (Int, String) where
    tooMany (n, _) = tooMany n

instance TooMany (Int, Int) where
    tooMany (x, y) = tooMany $ x + y

instance (Num a, TooMany a) => TooMany (a, a) where
    tooMany (x, y) = tooMany $ x + y

type AuthorName = String

-- Not in Normal Form
-- data Fiction = Fiction deriving Show
-- data Nonfiction = Nonfiction deriving Show
-- data BookType = FictionBook Fiction
--               | NonfictionBook Nonfiction
--               deriving Show
-- data Author = Author (AuthorName, BookType)

-- Normal Form
-- No further evaluation can be done of these constructors
-- until some operation or computation is done using these types
data Author =
    Fiction AuthorName
  | Nonfiction AuthorName
  deriving (Eq, Show)

type Gardener = String
-- data FlowerType = Gardenia
--                 | Daisy
--                 | Rose
--                 | Lilac
--                 deriving Show
-- data Garden =
--     Garden Gardener FlowerType
--     deriving Show

data Garden =
    Gardenia Gardener
  | Daisy Gardener
  | Rose Gardener
  | Lilac Gardener

data GuessWhat =
    Chickenbutt deriving (Eq, Show)
data Id a =
    MkId a deriving (Eq, Show)
data Product a b =
    Product a b deriving (Eq, Show)
data Sum a b =
    First a
  | Second b
  deriving (Eq, Show)

data RecordProduct a b =
    RecordProduct { pfirst :: a
                  , psecond :: b }
                  deriving (Eq, Show)

type Name = String
type Age = Int
type LovesMud = Bool
type PoundsOfWool = Int

data CowInfo =
    CowInfo Name Age
    deriving (Eq, Show)

data PigInfo =
    PigInfo Name Age LovesMud
    deriving (Eq, Show)

data SheepInfo =
    SheepInfo Name Age PoundsOfWool
    deriving (Eq, Show)

data Animal =
    Cow CowInfo
  | Pig PigInfo
  | Sheep SheepInfo
  deriving (Eq, Show)

type Animal' =
    Sum CowInfo (Sum PigInfo SheepInfo)

data OperatingSystem =
    GnuPlusLinux
  | OpenBSDPlusNevermindJustBSDStill
  | Mac
  | Windows
  deriving (Eq, Show)

data ProgrammingLanguage =
    Haskell
  | Agda
  | Idris
  | PureScript
  deriving (Eq, Show)

data Programmer =
    Programmer { os :: OperatingSystem
               , lang :: ProgrammingLanguage }
    deriving (Eq, Show)

allOperatingSystems :: [OperatingSystem]
allOperatingSystems = [GnuPlusLinux, OpenBSDPlusNevermindJustBSDStill, Mac, Windows]

allLanguages :: [ProgrammingLanguage]
allLanguages = [Haskell, Agda, Idris, PureScript]

allProgrammers :: [Programmer]
allProgrammers = [Programmer os pl | os <- allOperatingSystems, pl <- allLanguages]