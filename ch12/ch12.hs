notThe :: String -> Maybe String
notThe "the" = Nothing
notThe str = Just str

replaceThe :: String -> String
replaceThe str = unwords $ map (\x -> go x) $ map notThe $ words str where
    go x = case x of
            Nothing -> "a"
            Just x' -> x'

isVowelInitialWord :: String -> Bool
isVowelInitialWord w = isVowel $ head w

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel str = go 0 $ words str where
    go i [] = 0
    go i (x:xs) = if isVowelInitialWord x
                    then i
                    else if x == "the"
                        then go (i+1) xs
                        else go i xs

vowels = "aeiou"

isVowel :: Char -> Bool
isVowel c = elem c vowels

countVowels :: String -> Int
countVowels s = length $ filter isVowel s

newtype Word' = Word' String deriving (Eq, Show)

mkWord :: String -> Maybe Word'
mkWord s = if numVowels > numConsonants then Nothing else Just (Word' s) where
    numVowels = countVowels s
    numConsonants = length s - numVowels