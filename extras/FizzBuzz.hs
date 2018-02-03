import Data.Maybe (fromMaybe)
import Data.Foldable (fold)
import Control.Applicative (liftA2)

fizzBuzzNaive :: [Integer] -> [String]
fizzBuzzNaive = fmap convert where
    convert x
        | x `mod` 3 == 0 && x `mod` 5 == 0 = "FizzBuzz"
        | x `mod` 5 == 0 = "Buzz"
        | x `mod` 3 == 0 = "Fizz"

type FizzRule = Integer -> Maybe String

rule :: Integer -> String -> FizzRule
rule n m i =
    case i `mod` n of
         0 -> Just m
         _ -> Nothing

fizz = rule 3 "Fizz"
buzz = rule 5 "Buzz"

rules = [fizz, buzz]

fizzBuzz1 :: [Integer] -> [String]
fizzBuzz1 = fmap $ liftA2 fromMaybe show (mconcat . sequence rules)

-- Use Monoid m => Monoid (a -> m)
fizzBuzz2 :: [FizzRule] -> [Integer] -> [String]
fizzBuzz2 rules = fmap $ liftA2 fromMaybe show (fold rules)

-- Define rules using lambda
rules' = [(\x -> if x `mod` 3 == 0 then Just "Fizz" else Nothing),
          (\x -> if x `mod` 5 == 0 then Just "Buzz" else Nothing)]

fizzBuzz3 :: [Integer] -> [String]
fizzBuzz3 = fmap $ liftA2 fromMaybe show (mconcat . sequence rules')