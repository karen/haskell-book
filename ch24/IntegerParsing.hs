module IntegerParsing where

import Control.Applicative ((<|>))
import Text.Trifecta

parseDigit :: Parser Char
parseDigit = foldr (\x acc -> acc <|> char x) (char '0') ("123456789")

parseDigitAlt :: Parser Char
parseDigitAlt = oneOf ['0'..'0']

base10Integer :: Parser Integer
base10Integer = read <$> many parseDigit

base10Integer' :: Parser Integer
base10Integer' = do
    neg <- optional (char '-')
    int <- base10Integer
    return $ case neg of
        Nothing -> int
        Just _ -> -int

parseInt :: String -> Result Integer
parseInt = parseString base10Integer' mempty

main = do
    print $ parseInt "123"
    print $ parseInt "1"
    print $ parseInt "-123"
