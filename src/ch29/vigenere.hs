import Data.Maybe
import System.Environment
import System.Exit
import System.IO

import Cipher

type Key = String

main = do
    args <- getArgs
    key <- return $ args !! 0
    mode <- return $ if (args !! 1) == "-e" then Encrypt else Decrypt
    hasInput <- hWaitForInput stdin 5000
    if not hasInput
        then die "No input entered :("
    else
        do str <- hGetLine stdin
           let result = getResult mode key str
           hPutStrLn stdout result

getResult :: CipherType -> Key -> String -> String
getResult ct k str = case ct of
    Encrypt -> vigenere k str
    Decrypt -> unVigenere k str