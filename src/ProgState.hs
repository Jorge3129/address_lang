module ProgState where

import Data.List (intercalate, maximumBy)
import Data.Map (Map, toList)
import Data.Maybe (fromMaybe)
import Data.Ord

type MemoryState = Map Int Int

type VarState = Map String Int

data ProgState = ProgState MemoryState VarState

instance Show ProgState where
  show = ("\nProgState \n" ++) . formatTable . padTable . map (\(a, b, c) -> [a, b, c]) . serializeProgState

type LabelDict = Map String Int

type Table = [[String]]

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x : xs) = Just x

formatTable :: Table -> String
formatTable table =
  let rows = map (intercalate " | ") table
   in intercalate "\n" rows

serializeProgState :: ProgState -> [(String, String, String)]
serializeProgState = map (\(a, b, c) -> (a, show b, show c)) . zipProgState

zipProgState :: ProgState -> [(String, Int, Int)]
zipProgState (ProgState ms vs) =
  let varList = toList vs
      memList = toList ms
      lookupVal val xs = safeHead [(a, b) | (a, b) <- xs, b == val]
      getVarName addr = maybe "" fst (lookupVal addr varList)
   in [(getVarName addr, addr, val) | mem@(addr, val) <- memList]

padTable :: Table -> Table
padTable [] = []
padTable rows@(firstRow : _) =
  let cols = length firstRow
      colWidths = [maximum [length (row !! i) | row <- rows] | i <- [0 .. cols - 1]]
   in [ [ cell ++ replicate (colWidths !! colIndex - length cell) ' '
          | (cell, colIndex) <- zip cells [0 :: Int ..]
        ]
        | cells <- rows
      ]
