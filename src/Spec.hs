module Spec where

import AST
import Data.Map (fromList)
import Lang (readMem)
import ProgState (ProgState (ProgState))
import Run (runBaseFile, runBaseFile1)

myGcd :: Int -> Int -> IO Int
myGcd a b = do
  let resultAddr = 130
  resProgState@(ProgState ms _) <-
    runBaseFile1 "gcd_1" $
      ProgState
        (fromList [(10, 110), (20, 120), (30, 130), (110, a), (120, b)])
        (fromList [("alpha", 10), ("beta", 20), ("gamma", 30)])
  return $ readMem resultAddr ms

testGcd :: IO ()
testGcd = do
  foo <- runBaseFile "gcd1"
  return ()