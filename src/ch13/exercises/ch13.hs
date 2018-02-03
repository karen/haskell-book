import Control.Monad (forever)
import System.Exit (exitSuccess)
import Data.Char (toLower, isAlphaNum)

palindrome :: IO ()
palindrome = forever $ do
    line1 <- getLine
    case map toLower line1 == reverse (map toLower $ filter isAlphaNum line1) of
        True -> do
            putStrLn "It's a palindrome!"
            exitSuccess
        False -> do
            putStrLn "It's not a palindrome."
            exitSuccess

type Name = String
type Age = Integer
data Person = Person Name Age deriving Show
data PersonInvalid = NameEmpty
                   | AgeTooLow
                   | PersonInvalidUnknown String
                   deriving (Eq, Show)

mkPerson :: Name -> Age -> Either PersonInvalid Person
mkPerson name age
    | name /= "" && age > 0 = Right $ Person name age
    | name == "" = Left NameEmpty
    | not (age > 0) = Left AgeTooLow
    | otherwise = Left $ PersonInvalidUnknown $
                        "Name was: " ++ show name ++
                        " Age was: " ++ show age

gimmePerson :: IO ()
gimmePerson = do
    putStr "Name: "
    name <- getLine
    putStr "Age: "
    ageStr <- getLine
    case mkPerson name $ read ageStr of
        Left e -> do putStrLn $ show e
        Right p -> do putStrLn $ "Yay! Successfully got a person: " ++ show p
