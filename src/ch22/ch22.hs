{-# LANGUAGE InstanceSigs #-}

import Data.Char
import Control.Applicative (liftA2)

cap :: [Char] -> [Char]
cap xs = map toUpper xs

rev :: [Char] -> [Char]
rev xs = reverse xs

composed :: [Char] -> [Char]
composed = cap . rev

fmapped :: [Char] -> [Char]
fmapped = fmap cap rev

-- tupled = (,) <$> cap <*> rev
tupled :: String -> (String, String)
tupled = liftA2 (,) cap rev

tupled' :: String -> (String, String)
tupled' = do
    r <- rev
    c <- cap
    return (c, r)

tupled'' :: String -> (String, String)
tupled'' = rev >>=
            \reversed -> cap >>=
                \capitalised -> return (reversed, capitalised)

newtype Reader r a =
    Reader { runReader :: r -> a }

instance Functor (Reader r) where
    fmap ab (Reader ra) = 
        Reader $ \r -> ab (ra r)

ask :: Reader a a
ask = Reader id

newtype HumanName = HumanName String deriving (Eq, Show)
newtype DogName = DogName String deriving (Eq, Show)
newtype Address = Address String deriving (Eq, Show)

data Person =
    Person {
        humanName :: HumanName
      , dogName :: DogName
      , address :: Address
      } deriving (Eq, Show)

data Dog =
    Dog {
    dogsName :: DogName
  , dogsAddress :: Address
  } deriving (Eq, Show)

pers :: Person
pers =
    Person (HumanName "Doctor Who")
           (DogName "Barkley")
           (Address "Universe")

chris :: Person
chris = Person (HumanName "Chris")
               (DogName "Doggo")
               (Address "Earth")

getDog :: Person -> Dog
getDog p = Dog (dogName p) (address p)

getDogR :: Person -> Dog
getDogR =
    Dog <$> dogName <*> address

getDogR' :: Person -> Dog
getDogR' = liftA2 Dog dogName address

myLiftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
myLiftA2 f a b = f <$> a <*> b

asks :: (r -> a) -> Reader r a
asks = Reader

instance Applicative (Reader r) where
    pure :: a -> Reader r a
    pure a = Reader $ const a

    (<*>) :: Reader r (a -> b) -> Reader r a -> Reader r b
    (Reader rab) <*> (Reader ra) = Reader $ \r -> (rab r) (ra r)

getDogR'' :: Reader Person Dog
getDogR'' = Reader (liftA2 Dog dogName address)

instance Monad (Reader r) where
    return = pure

    (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b
    (Reader ra) >>= aRb = Reader $ \r -> runReader (aRb (ra r)) r

getDogRM :: Reader Person Dog
getDogRM = Reader (dogName) >>= (\name -> (Reader (\p -> Dog name (address p))))
