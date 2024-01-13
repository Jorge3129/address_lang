module Main where

import Run
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  let fileName = head args
  runFile fileName