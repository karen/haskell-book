type Numerator = Integer
type Denominator = Integer
data DividedResult =
    Result Integer
  | DividedByZero
  deriving Show

dividedBy :: Numerator -> Denominator -> DividedResult
dividedBy num 0 = DividedByZero
dividedBy num denom
    | (num < 0) /= (denom < 0) = Result $ negate $ fst ans
    | otherwise = Result $ fst ans
    where go n d count
            | n < d = (count, n)
            | otherwise = go (n - d) d (count + 1)
          ans = go (abs num) (abs denom) 0

s :: (Eq a, Num a) => a -> a -> a
s 0 acc = acc
s x acc = s (x-1) (acc+x)

mySum :: (Eq a, Num a) => a -> a
mySum x = s x 0

mc91 n =
    case n > 100 of
        True -> n-10
        False -> mc91 $ mc91(n+11)
