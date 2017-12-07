import Data.Char

data CipherType = Encrypt | Decrypt
    deriving (Eq, Show)

caesar :: Int -> String -> String
caesar _ [] = []
caesar n x = caesarHelper n x Encrypt

unCaesar :: Int -> String -> String
unCaesar _ [] = []
unCaesar n x = caesarHelper n x Decrypt

caesarHelper :: Int -> String -> CipherType -> String
caesarHelper _ [] _ = []
caesarHelper n (x:xs) ctype = chr (ord base + (mod (offset ctype x base n) 26)) : caesarHelper n xs ctype where
    base = if compare x 'a' == GT then 'a' else 'A'

offset :: CipherType -> Char -> Char -> Int -> Int
offset Encrypt x base n = (+) (ord x - ord base) n
offset Decrypt x base n = (-) (ord x - ord base) n

vigenere :: String -> String -> String
vigenere c x = go 0 c x where
    go _ _ [] = []
    go i c (' ':xs) = ' ' : go (mod (i) (length c)) c xs
    go i c (x:xs) = (caesar (ord (c !! i) - ord base) [x]) ++ go (mod (i+1) (length c)) c xs where
        base = if compare x 'a' == GT then 'a' else 'A'

isSubsequenceOf :: (Eq a) => [a] -> [a] -> Bool
isSubsequenceOf [] _ = True
isSubsequenceOf _ [] = False
isSubsequenceOf s@(x:xs) (y:ys) =
    case x == y of
        True -> isSubsequenceOf xs ys
        False -> isSubsequenceOf s ys

capitalize :: String -> String
capitalize [] = []
capitalize (x:xs) = toUpper x : xs

capitalizeWords :: String -> [(String, String)]
capitalizeWords str = map (\x -> (x, capitalize x)) wrds where
    wrds = words str

capitalizeParagraph :: String -> String
capitalizeParagraph w = go w True where
    go [] _ = []
    go ('.':xs) _ = '.' : go xs True
    go (' ':xs) True = ' ' : go xs True
    go (x:xs) True = toUpper x : go xs False
    go (x:xs) False = x : go xs False