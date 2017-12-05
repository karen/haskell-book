f :: String -> [String]
f "" = []
f x = takeWhile (/= ' ') x : f (dropWhile (== ' ') $ dropWhile (/= ' ') x)

g :: String -> Char -> [String]
g "" _ = []
g x delim = takeWhile (/= delim) x : f (dropWhile (== delim) $ dropWhile (/= delim) x)

mySqr = [x^2 | x <- [1..5]]
myCube = [y^3 | y <- [1..5]]

tups = [(sq, cube) | sq <- mySqr, cube <- myCube]
tups' = [(sq, cube) | sq <- mySqr, cube <- myCube, sq < 50, cube < 50]

-- [1,2,3,4,5] WHNF, NF
-- 1 : 2 : 3 : 4 : _ WHNF
-- enumFromTo 1 10 WHNF: Wrong -> Neither `enum...` is not a data constructor
-- length [1,2,3,4,5] WHNF: Wrong -> Neither
-- sum (enumFromTo 1 10) WHNF: Wrong -> Neither
-- ['a'..'m'] ++ ['n'..'z'] Neither
-- (_, 'b') WHNF

multp3 = length . filter (\x -> x `mod` 3 == 0)

myFilter :: String -> [String]
myFilter str = filter (\x -> not (elem x ["the", "a", "an"])) $ words str

myZip :: [a] -> [b] -> [(a, b)]
myZip _ [] = []
myZip [] _ = []
myZip (x:xs) (y:ys) = (x,y) : myZip xs ys

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith _ _ [] = []
myZipWith _ [] _ = []
myZipWith f (x:xs) (y:ys) = f x y : myZipWith f xs ys

id' :: (a -> b -> (a,b))
id' x y = (x,y)

zip' xs ys = myZipWith id' xs ys