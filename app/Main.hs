module Main where

import Lib
import System.Environment

main :: IO ()
main = do
  [filepath] <- getArgs
  file <- readFile filepath
  runInterpreter $ read file
