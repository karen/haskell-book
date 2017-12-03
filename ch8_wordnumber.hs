module WordNumber where

import Data.List (intersperse)

digitToWord :: Int -> String
digitToWord 0 = "zero"
digitToWord 1 = "one"
digitToWord 2 = "two"
digitToWord 3 = "three"
digitToWord 4 = "four"
digitToWord 5 = "five"
digitToWord 6 = "six"
digitToWord 7 = "seven"
digitToWord 8 = "eight"
digitToWord 9 = "nine"

digits :: Int -> [Int]
digits n = go n []
    where go n acc
            | n == 0 = acc
            | otherwise = go (div n 10) ((mod n 10) : acc)

wordNumber :: Int -> String
wordNumber 0 = "zero"
wordNumber n = concat $ intersperse "-" $ map digitToWord $ digits n