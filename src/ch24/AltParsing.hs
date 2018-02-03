{-# LANGUAGE QuasiQuotes #-}

module AltParsing where

import Control.Applicative
import Text.RawString.QQ
import Text.Trifecta

import Data.Ratio ((%))

type NumberOrString = Either Integer String

a = "blah"
b = "123"
c = "123blah789"

parseNos :: Parser NumberOrString
parseNos = do
    skipMany (oneOf "\n")
    v <- (Left <$> integer) <|> (Right <$> some letter)
    skipMany (oneOf "\n")
    return v

parseFraction :: Parser Rational
parseFraction = do
    numerator <- decimal
    char '/'
    denominator <- decimal
    return (numerator % denominator)

type DecOrFrac = Either Float Rational

parseDecimal :: Parser Float
parseDecimal = do
    base <- decimal
    char '.'
    rest <- decimal
    return $ (fromIntegral base) + (convertDec rest)

convertDec :: Integer -> Float
convertDec x = go (fromIntegral x) where
    go x
        | x < 1 = x
        | otherwise = go (x / 10)

parseDecOrFrac :: Parser DecOrFrac
parseDecOrFrac = 
    (Left <$> try parseDecimal)
    <|> (Right <$> parseFraction)

eitherOr :: String
eitherOr = [r|
123
abc
456
def
|]

main = do
    print $ parseString (some letter) mempty a
    print $ parseString integer mempty b
    print $ parseString parseNos mempty a
    print $ parseString parseNos mempty b
    print $ parseString (many parseNos) mempty c
    print $ parseString (some parseNos) mempty c
