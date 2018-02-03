{-# LANGUAGE QuasiQuotes #-}

import Control.Applicative ((<|>))
import Text.Trifecta
import Text.RawString.QQ

type LogFile = [LogDay]
type LogDay = (Date, [LogLine])

type Year = Integer
type Month = Integer
type Day = Integer

data Date = Date Year Month Day

type Hour = Integer
type Minute = Integer
data Time = Time Hour Minute

type Log = String
data LogLine = Line Time Log

instance Show Date where
    show (Date y m d) = show y ++ "-" ++ show m ++ "-" ++ show d

instance Show Time where
    show (Time h m) = pad 2 (show h) ++ ":" ++ pad 2 (show m)

instance Show LogLine where
    show (Line t l) = show t ++ " " ++ l

pad :: Int -> String -> String
pad n s = if (length s) >= n then s else pad n ('0':s)

parseLogFile :: Parser LogFile
parseLogFile = spaces >> many parseLogDay <* eof

parseLogDay :: Parser LogDay
parseLogDay = do
    date <- parseDate
    lines <- many parseLogLine
    return $ (date, lines)

parseDate :: Parser Date
parseDate = do
    char '#' >> spaces
    year <- parseYear
    spaces >> char '-' >> spaces
    month <- parseMonth
    spaces >> char '-' >> spaces
    day <- parseDay
    skipMany comments
    return $ Date year month day

parseYear :: Parser Year
parseYear = natural

parseMonth :: Parser Month
parseMonth = natural

parseDay :: Parser Day
parseDay = natural

comments :: Parser [Char]
comments = do
        spaces >> string "--"
        anyCharTillEnd

parseLogLine :: Parser LogLine
parseLogLine = do
    time <- parseTime
    log <- parseLog
    return $ Line time log

parseTime :: Parser Time
parseTime = do
    hour <- natural
    colon
    min <- natural
    return $ Time hour min

parseLog :: Parser Log
parseLog = try lineWithComment <|> anyCharTillEnd

lineWithComment :: Parser Log
lineWithComment = manyTill (anyChar <* notFollowedBy newline) (string "--")
               <* (try anyCharTillEnd <|> manyTill anyChar eof)

anyCharTillEnd :: Parser Log
anyCharTillEnd = manyTill anyChar newline

test :: String -> Result LogFile
test = parseString parseLogFile mempty

oneDay :: String
oneDay = [r|# 2018-01-01
08:00 Wake up
09:00 Really wake up
10:00 Breakfast
11:00 Watch Doctor Who!
12:00 Lunch
13:00 Nap -- Test comment|]

example :: String
example = [r|
# 2025-02-05
08:00 Breakfast
09:00 Sanitizing moisture collector
11:00 Exercising in high-grav gym
12:00 Lunch
13:00 Programming
17:00 Commuting home in rover
17:30 R&R
19:00 Dinner
21:00 Shower
21:15 Read
22:00 Sleep
# 2025-02-07 -- dates not nececessarily sequential
08:00 Breakfast -- should I try skippin bfast?
09:00 Bumped head, passed out
13:36 Wake up, headache
13:37 Go to medbay
13:40 Patch self up
13:45 Commute home for rest
14:15 Read
21:00 Dinner
21:15 Read
22:00 Sleep -- last
|]

main = do
    print $ test oneDay
    print $ test example
