functionC :: Ord a => a -> a -> a
functionC x y =
    case x > y of
        True -> x
        False -> y

ifEvenAdd2 :: Integral a => a -> a
ifEvenAdd2 n =
    case even n of
        True -> n + 2
        False -> n

nums x =
    case compare x 0 of
        LT -> -1
        GT -> 1
        EQ -> 0

tensDigit :: Integral a => a -> a
tensDigit x = snd ((fst $ x `divMod` 10) `divMod` 10)

hunsD x = d2
    where d1 = fst (x `divMod` 10)
          d2 = tensDigit d1

foldBool :: a -> a -> Bool -> a
foldBool x y z =
    case z of
        True -> x
        False -> y

foldBool' :: a -> a -> Bool -> a
foldBool' x y True = x
foldBool' x y False = y

g :: (a -> b) -> (a, c) -> (b, c)
g f (a, c) = (f a, c)