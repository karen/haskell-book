module SemVerParsing where

import Control.Applicative ((<|>))
import Text.Trifecta

import Test.Hspec

data NumberOrString =
    NOSS String
  | NOSI Integer
  deriving (Show, Eq)

type Major = Integer
type Minor = Integer
type Patch = Integer
type Release = [NumberOrString]
type Metadata = [NumberOrString]

data SemVer =
    SemVer Major Minor Patch Release Metadata
    deriving (Show, Eq)

instance Ord SemVer where
    compare (SemVer ma1 mi1 p1 _ _) (SemVer ma2 mi2 p2 _ _) =
        case compare ma1 ma2 of
            EQ -> case compare mi1 mi2 of
                EQ -> compare p1 p2
                ord -> ord
            ord -> ord

parseSemVer :: Parser SemVer
parseSemVer = do
    major <- natural
    minor <- (char '.') >> natural
    patch <- (char '.') >> natural
    release <- (return [] <* eof) <|> (parseInfo '-') <|> return []
    metadata <- (return [] <* eof) <|> (parseInfo '+')
    return $ SemVer major minor patch release metadata

parseInfo :: Char -> Parser [NumberOrString]
parseInfo c = (try (char c)) >> alphaNumDotSeparated

alphaNumDotSeparated :: Parser [NumberOrString]
alphaNumDotSeparated = sepBy parseNumOrString $ char '.'

parseNumOrString :: Parser NumberOrString
parseNumOrString = (NOSS <$> try (some letter))
               <|> (NOSI <$> integer)

parseSemVer' :: String -> Result SemVer
parseSemVer' = parseString parseSemVer mempty

main :: IO ()
main = do
    print $ parseSemVer' "0.0.0"
    print $ parseSemVer' "0.10.0"
    print $ parseSemVer' "0.1.0-alpha"
    print $ parseSemVer' "0.1.0-alpha.beta.1"
    print $ parseSemVer' "0.1.0+foo"
    print $ parseSemVer' "0.1.0+foo.bar.1"
    print $ parseSemVer' "0.1.0-alpha.beta.1+foo.bar"
    print $ parseSemVer' "-1.0.0"
    print $ parseSemVer' "0.1.0.."
    print $ parseSemVer' "0.1.0-alpha.."
    print $ parseSemVer' "0.1.0.-alpha"
