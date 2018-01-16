module LearnParsers where

import Text.Trifecta

stop :: Parser Char
stop = unexpected "stop"

one :: Parser Char
one = char '1'

oneEof :: Parser Char
oneEof = char '1' <* eof

one' :: Parser Char
one' = one >> stop

oneTwo :: Parser Char
oneTwo = char '1' >> char '2'

oneTwo' :: Parser Char
oneTwo' = oneTwo >> stop

testParse :: (Show a) => Parser a -> IO ()
testParse p = print $ parseString p mempty "12"

str1 :: Parser String
str1 = string "1"

str12 :: Parser String
str12 = string "12"

str123 :: Parser String
str123 = string "123"

string' :: String -> Parser String
string' [] = return []
string' (x:xs) = do
    st <- char x
    rest <- string' xs
    return $ st : rest

pNL s = putStrLn ('\n' : s)

main = do
    pNL "stop:"
    testParse stop
    pNL "one:"
    testParse one
    pNL "one':"
    testParse one'
    pNL "oneTwo:"
    testParse oneTwo
    pNL "oneTwo':"
    testParse oneTwo'
    testParse (one >> eof)
    testParse (oneTwo >> eof)