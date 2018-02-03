isJust :: Maybe a -> Bool
isJust Nothing = False
isJust (Just _) = True

isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing (Just _) = False

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee acc _ Nothing = acc
mayybee acc f (Just x) = f x

fromMaybe :: a -> Maybe a -> a
fromMaybe fb Nothing = fb
fromMaybe _ (Just x) = x

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x:_) = Just x

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just x) = [x]

catMaybes :: [Maybe a] -> [a]
catMaybes x = foldr (\x acc -> maybeToList x ++ acc) [] (filter isJust x)

flipMaybe :: Eq a => [Maybe a] -> Maybe [a]
flipMaybe xs = case elem Nothing xs of
                True -> Nothing
                False -> Just (catMaybes xs)
