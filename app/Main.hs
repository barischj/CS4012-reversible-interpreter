module Main where

import Lib
import System.Environment

main :: IO ()
main = do
  [filepath, info] <- getArgs
  file <- readFile filepath
  runInterpreter (read file) $ read info
