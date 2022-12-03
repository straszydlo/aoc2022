module Main (main) where

import Lib
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  someFunc $ head args

