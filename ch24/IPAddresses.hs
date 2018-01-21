import Data.Word
import Data.Char (ord, toUpper)

import Control.Applicative ((<|>))
import Text.Trifecta

data IPAddress = IPAddress Word32 deriving (Eq, Ord, Show)
data IPAddress6 = IPAddress6 Word64 Word64 deriving (Eq, Ord)

instance Show IPAddress6 where
    show (IPAddress6 hi lo) = show $ (fromIntegral hi) * 2^64 + fromIntegral lo

parseIPv4 :: Parser IPAddress
parseIPv4 = do
    w <- natural
    dot
    x <- natural
    dot
    y <- natural
    dot
    z <- natural
    eof
    return $ IPAddress (ipToWord w x y z)

parseIPv6 :: Parser IPAddress6
parseIPv6 = do
    res <- some $ try (manyTill (hex) colon) <|> some hex
    let hi = padr 4 . extractHi $ res
        lo = padl 4 . reverse . extractHi . reverse $ res
    return $ IPAddress6 (strsToWords hi) (strsToWords lo)

extractHi :: [String] -> [String]
extractHi = take 4 . takeWhile (/= "")

padl :: Int -> [String] -> [String]
padl n s = if length s >= n then s else padl n ("0" : s)

padr :: Int -> [String] -> [String]
padr n s = if length s >= n then s else padr n (s ++ ["0"])

strsToWords :: [String] -> Word64
strsToWords s = numsToWord (map hexToNum s)

hexToNum :: [Char] -> Integer
hexToNum = foldl (\acc x -> acc * 16 + hexToDigit (toUpper x)) 0

hexToDigit :: Char -> Integer
hexToDigit 'A' = 10
hexToDigit 'B' = 11
hexToDigit 'C' = 12
hexToDigit 'D' = 13
hexToDigit 'E' = 14
hexToDigit 'F' = 15
hexToDigit x = fromIntegral $ (ord x - ord '0')

numsToWord :: [Integer] -> Word64
numsToWord = fromInteger . foldl (\acc x -> acc * (2^16) + x) 0

hexdigits :: String
hexdigits = "0123456789ABCDEFabcdef"

hex :: Parser Char
hex = oneOf hexdigits

ipToWord :: Integer -> Integer -> Integer -> Integer -> Word32
ipToWord w x y z = fromInteger $ z + y * 256 + x * 256 * 256 + w * 256 * 256 * 256

testIPv4 :: String -> Result IPAddress
testIPv4 = parseString parseIPv4 mempty

test :: String -> Result IPAddress6
test = parseString parseIPv6 mempty

main = do
    print $ testIPv4 "172.16.254.1"
    print $ testIPv4 "204.120.0.15"
    print $ testIPv4 "0.0.1.1"
    print $ testIPv4 "192.168.0.1"
    print $ test "0:0:0:0:0:ffff:ac10:fe01"
    print $ test "0:0:0:0:0:ffff:cc78:f"
    print $ test "FE80:0000:0000:0000:0202:B3FF:FE1E:8329"
    print $ test "FE80::0202:B3FF:FE1E:8329"
    print $ test "2001:DB8::8:800:200C:417A"
