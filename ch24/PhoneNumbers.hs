import Control.Applicative ((<|>))
import Data.Char (ord)
import Text.Trifecta

type NumberingPlanArea = Int
type Exchange = Int
type LineNumber = Int

data PhoneNumber = 
    PhoneNumber NumberingPlanArea Exchange LineNumber
    deriving (Eq, Show)

parsePhone :: Parser PhoneNumber
parsePhone = do
    parseWithCCode <|> parseStandard

parseWithCCode :: Parser PhoneNumber
parseWithCCode = do
    try $ do 
        char '1'
        char '-'
    parseStandard

parseStandard :: Parser PhoneNumber
parseStandard = do
    npa <- parseXDigits 3
    exc <- parseXDigits 3
    lin <- parseXDigits 4
    eof
    return (PhoneNumber npa exc lin)

phoneDigit :: Parser Char
phoneDigit = skipMany (oneOf "-() ") >> digit

parseXDigits :: Int -> Parser Int
parseXDigits x = go x (return 0) where
    go :: Int -> Parser Int -> Parser Int
    go 0 p = p
    go n p = do
        d <- (\x -> x - ord '0') . ord <$> phoneDigit
        go (n-1) ((d+) . (10*) <$> p)

test :: String -> Result PhoneNumber
test = parseString parsePhone mempty
