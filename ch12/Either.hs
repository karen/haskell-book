lefts' :: [Either a b] -> [a]
lefts' = foldr (\x acc -> go x ++ acc) [] where
    go (Left x') = [x']
    go _ = []

rights' :: [Either a b] -> [b]
rights' = foldr (\x acc -> go x ++ acc) [] where
    go (Right x') = [x']
    go _ = []

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' xs = (lefts' xs, rights' xs)

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' _ (Left _) = Nothing
eitherMaybe' f (Right x) = Just (f x)

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f _ (Left x) = f x
either' _ g (Right x) = g x

eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' _ (Left _) = Nothing
eitherMaybe'' f r = either' (\x -> Nothing) (Just . f) r
