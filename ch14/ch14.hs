module Main where

import Data.Char
import Test.QuickCheck

half x = x / 2

halfIdentity = (*2) . half

prop_floatsHalved :: Property
prop_floatsHalved =
    forAll (arbitrary :: Gen Float)
    (\x -> half x == x / 2)

prop_identity :: Property
prop_identity =
    forAll (arbitrary :: Gen Float)
    (\x -> halfIdentity x == x)

plusAssociative :: Int -> Int -> Int -> Bool
plusAssociative x y z = x + (y + z) == (x + y) + z

plusCommutative :: Int -> Int -> Bool
plusCommutative x y = x + y == y + x

powNotAssociative :: Int -> Int -> Int -> Bool
powNotAssociative x y z = (x^y)^z == x^(y^z)

f :: String -> Bool
f x = (read (show x)) == x

twice f = f . f
fourTimes = twice . twice

capitalizeWord :: String -> String
capitalizeWord w
    | null w = w
    | otherwise = [toUpper firstLetter] ++ map toLower others where ([firstLetter], others) = splitAt 1 w

g :: String -> Bool
g x =
    (capitalizeWord x == twice capitalizeWord x) && (capitalizeWord x == fourTimes capitalizeWord x)

main :: IO ()
main = quickCheck g

data Fool = Fulse
          | Frue
          deriving (Eq, Show)

genEqualFool :: Gen Fool
genEqualFool = elements [Fulse, Frue]

genUnequalFool :: Gen Fool
genUnequalFool = 
    frequency [(1, return Frue),
               (2, return Fulse)]
