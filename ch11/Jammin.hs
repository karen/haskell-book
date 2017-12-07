module Jammin where

import Data.List


data Fruit =
    Peach
  | Plum
  | Apple
  | Blackberry
  deriving (Eq, Show, Ord)

-- data JamJars =
--     Jam Fruit Int
--     deriving (Eq, Show)

data JamJars =
    Jam { fruit :: Fruit
        , jars :: Int}
        deriving (Eq, Show, Ord)

row1 = Jam Peach 3
row2 = Jam Plum 5
row3 = Jam Apple 2
row4 = Jam Blackberry 4
row5 = Jam Peach 3
row6 = Jam Apple 1
allJam = [row1, row2, row3, row4, row5, row6]

totalNumJars :: [JamJars] -> Int
totalNumJars = foldr (\x acc -> jars x + acc) 0

mostRow :: [JamJars] -> JamJars
mostRow (x:xs) = foldr (\x acc -> if jars x > jars acc then x else acc) x (x:xs)

compareKind :: JamJars -> JamJars -> Ordering
compareKind (Jam k _) (Jam k' _) = compare k k'

sortedJams = sortBy compareKind allJam

groupedJams = groupBy (\x x' -> fruit x == fruit x') sortedJams