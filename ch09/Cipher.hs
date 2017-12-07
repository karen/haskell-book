module Cipher where

import Data.Char

filterUpper :: String -> String
filterUpper = filter isUpper

capitalize :: String -> String
capitalize [] = []
capitalize (x:xs) = toUpper x : xs

capitalize' :: String -> String
capitalize' [] = []
capitalize' (x:xs) = toUpper x : capitalize' xs

firstCapitalized :: String -> Char
firstCapitalized x = head $ capitalize x

firstCapitalized' :: String -> Char
firstCapitalized' = toUpper . head

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

testEncrypt :: IO ()
testEncrypt =
  if caesar 3 "ABC" == "DEF"
  then putStrLn "Ok"
  else putStrLn "No wat"

testCipher :: IO ()
testCipher =
    if (unCaesar 10 $ caesar 10 "ABCDEF") == "ABCDEF"
    then putStrLn "Ok"
    else putStrLn "Encryption failed"

main :: IO ()
main = do
  testEncrypt
  testCipher
