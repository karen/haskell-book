import Data.Char
import Data.List

convo =
    ["Wanna play 20 questions",
     "Ya",
     "U 1st haha",
     "Lol ok. Have u ever tasted alcohol lol",
     "Lol ya",
     "Wow ur cool haha. Ur turn",
     "Ok. Do u think I am pretty Lol",
     "Lol ya",
     "Haha thanks just making sure rofl ur turn"]
-- validButtons = "1234567890*#"
type Digit = Char
-- Valid presses: 1 and up
type Presses = Int

type KeyValue = String

data DaPhone = DaPhone [(Digit, KeyValue)] deriving (Eq, Show)

phone = DaPhone [('2', "abc"), ('3', "def"), ('4', "ghi"), ('5', "jkl"),
                 ('6', "mno"), ('7', "pqrs"), ('8', "tuv"), ('9', "wxyz"),
                 ('*', "^"), ('0', "+_"), ('#', ".,")]

-- assuming the default phone definition
-- 'a' -> ('2', 1)
-- 'A' -> [('*', 1), ('2', 1)]
reverseTaps :: DaPhone -> Char -> [(Digit, Presses)]
reverseTaps p@(DaPhone x) c
    | isUpper c = [('*', 1)] ++ reverseTaps p (toLower c)
    | isDigit c = [(c, 1)]
    | isSpace c = [('0', 1)]
    | isUpper c == False = [(fst key, index key c)]
    where key = filter (\x -> elem c (snd x)) x !! 0

index :: (Digit, KeyValue) -> Char -> Presses
index (_, x) c = go x c 1 where
    go (x:xs) c i = case c == x of
                    True -> i
                    False -> go xs c (i+1)

cellPhonesDead :: DaPhone
               -> String
               -> [(Digit, Presses)]
cellPhonesDead p = foldl (\acc x -> acc ++ reverseTaps p x) []

fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps = sum . map snd

f :: Ord b => ((a, b) -> (a, b) -> Ordering)
f x x' = compare (snd x) (snd x')

mostPopularLetter :: String -> Char
mostPopularLetter str = fst key
    where key = maximumBy f $ mostPopFreq str

mostPopFreq :: String -> [(Char, Int)]
mostPopFreq str = [((x !! 0), length x) | x <- groupBy (==) str]

popularLetterCost :: DaPhone -> String -> Presses
popularLetterCost p str = fingerTaps allPresses
    where mpl  = mostPopularLetter str
          str' = filter (\x -> x == mpl) str
          allPresses = cellPhonesDead p str'

coolestLtr :: [String] -> Char
coolestLtr strs = mostPopularLetter $ concat strs
