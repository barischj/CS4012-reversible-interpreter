module Main where

import Lib (runInterpreter)
import Static (analyse, printErrs)
import System.Environment

-- | Runs the interpreter on a file.
-- If the file doesn't parse or has static analysis errors then the interpreter
-- will NOT be run.
main :: IO ()
main = do
    [filepath] <- getArgs
    file <- readFile filepath
    let stmt = read file
    errs <- analyse stmt
    case errs of
        [] -> runInterpreter stmt
        _  -> printErrs errs
  
