module Run where

import AST
import Data.Map qualified
import Lang
import Parse
import ProgState (ProgState (ProgState))
import System.Environment
import System.IO

runFile :: FilePath -> IO ProgState
runFile fileName = do
  progTree <- parseOrThrow parseProg <$> readFile fileName
  print progTree
  runProgram progTree $ ProgState Data.Map.empty Data.Map.empty

runFile1 :: FilePath -> ProgState -> IO ProgState
runFile1 fileName ps = do
  progTree <- parseOrThrow parseProg <$> readFile fileName
  runProgram progTree ps

runBaseFile :: String -> IO ProgState
runBaseFile fileName = runFile ("../text/" ++ fileName ++ ".txt")

runBaseFile1 :: String -> ProgState -> IO ProgState
runBaseFile1 fileName = runFile1 ("../text/" ++ fileName ++ ".txt")