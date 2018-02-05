module Cipher where

import Data.Char
import Test.QuickCheck

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
caesarHelper n (' ':xs) ctype = ' ' : caesarHelper n xs ctype
caesarHelper n (x:xs) ctype = chr (ord base + (mod (offset ctype x base n) 26)) : caesarHelper n xs ctype where
    base = if compare x 'a' /= LT then 'a' else 'A'

offset :: CipherType -> Char -> Char -> Int -> Int
offset Encrypt x base n = (+) (ord x - ord base) n
offset Decrypt x base n = (-) (ord x - ord base) n

vigenere :: String -> String -> String
vigenere = vigenereHelper Encrypt

unVigenere :: String -> String -> String
unVigenere = vigenereHelper Decrypt

vigenereHelper :: CipherType -> String -> String -> String
vigenereHelper ct c x = go 0 c x where
    go _ _ [] = []
    go i c (' ':xs) = ' ' : go (mod (i) (length c)) c xs
    go i c (x:xs) = (f (ord (c !! i) - ord base) [x]) ++ go (mod (i+1) (length c)) c xs where
        f = case ct of
            Encrypt -> caesar
            Decrypt -> unCaesar
        base = if compare x 'a' == GT then 'a' else 'A'