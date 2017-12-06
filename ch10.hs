import Data.Time

fibs = 1 : scanl (+) 1 fibs
fibsN x = fibs !! x

firstKFibs = take 20 fibs

smallFibs = takeWhile (< 100) fibs

fact = 1 : scanl (*) 1 [1..]
data DatabaseItem = DbString String
                  | DbNumber Integer
                  | DbDate UTCTime
                  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
    [ DbDate (UTCTime
                (fromGregorian 1911 5 1)
                (secondsToDiffTime 34123))
    , DbNumber 9001
    , DbString "Hello, world!"
    , DbDate (UTCTime
                (fromGregorian 1921 5 1)
                (secondsToDiffTime 34123))
    ]

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate xs = foldr go [] xs
    where go (DbDate x) ys = x : ys
          go _ ys          = ys

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber xs = foldr go [] xs
    where go (DbNumber x) ys = x : ys
          go _ ys            = ys

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent xs = foldr go st dates
    where dates = filterDbDate xs
          st = dates !! 0
          go a b = if a < b then b else a

sumDb :: [DatabaseItem] -> Integer
sumDb xs = foldr (+) 0  (filterDbNumber xs)

sumDb' :: [DatabaseItem] -> Integer
sumDb' xs = sum $ filterDbNumber xs

avgDb :: [DatabaseItem] -> Double
avgDb xs = fromIntegral total / fromIntegral count
    where total = sumDb xs
          count = length $ filterDbNumber xs

stops = "pbtdkg"
vowels = "aeiou"

combi = [(x, y, z) | x <- stops, y <- vowels, z <- stops]

beginsP = filter go combi
    where go (fst, _, _) = fst == 'p'

nouns = ["boy", "cat", "dog"]
verbs = ["run", "jump", "fly"]

combi2 = [(x, y, z) | x <- nouns, y <- verbs, z <- nouns]

triplets xs ys = [(x, y, z) | x <- xs, y <- ys, z <- xs ]
combi3 = triplets nouns verbs

seekritFunc x = (/) totalNumChar numWords
    where totalNumChar = fromIntegral (sum (map length (words x)))
          numWords     = fromIntegral (length (words x))

myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs) =
  if x == True
  then True
  else myOr xs

myOr' [] = False
myOr' (x:xs) = x || myOr' xs

myOr'' :: [Bool] -> Bool
myOr'' = foldr (\a b ->
                if a == True
                then True
                else b) False

myOr''' :: [Bool] -> Bool
myOr''' = foldr (||) False

myAny :: (a -> Bool) -> [a] -> Bool
myAny f = myOr . map f

myElem'' :: Eq a => a -> [a] -> Bool
myElem'' _ [] = False
myElem'' x' (x:xs) =
  if x == x'
  then True
  else myElem'' x' xs

myElem' :: Eq a => a -> [a] -> Bool
myElem' x' = foldr (\x acc ->
                    if x == x'
                    then True
                    else acc) False

myElem :: Eq a => a -> [a] -> Bool
myElem x' = any (\x -> x == x')

myReverse' :: [a] -> [a]
myReverse' [] = []
myReverse' (x:xs) = myReverse' xs ++ [x]

myReverse :: [a] -> [a]
myReverse = foldl (\acc x -> x : acc) []

myReverse'' :: [a] -> [a]
myReverse'' = foldr (\x acc -> acc ++ [x]) []

myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr (\x acc -> f x : acc) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter pred = foldr (\x acc -> if pred x then x : acc else acc) []

squish :: [[a]] -> [a]
squish = foldr (\x acc -> x ++ acc) []

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = squish . map f

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy cmp (x:xs) = foldl go x (x:xs) where
  go acc x = case cmp acc x of
    GT -> acc
    LT -> x
    EQ -> acc

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy cmp (x:xs) = foldl go x (x:xs) where
  go acc x = case cmp acc x of
    GT -> x
    LT -> acc
    EQ -> acc
