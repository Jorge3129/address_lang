{-# OPTIONS_GHC -Wall #-}

module Lang where

import AST
import Data.Function (on)
import Data.List (sortBy)
import Data.Maybe (fromMaybe, isJust)

type MemoryState = [(Int, Int)]

type VarState = [(String, Int)]

type ProgramState = (MemoryState, VarState)

type LabelDict = [(String, Int)]

evalExp :: Expr -> ProgramState -> Int
evalExp (Lit val) _ = val
--
evalExp (Var name) (ms, vs) = case lookup name vs of
  Just addr -> case lookup addr ms of
    Just val -> val
    Nothing -> error ("Variable " ++ name ++ " has no value in memory.")
  Nothing -> error ("Variable " ++ name ++ " is not defined.")
--
evalExp (BinOpApp op rhs lhs) ps = case op of
  Add -> evalExp lhs ps + evalExp rhs ps
  Sub -> evalExp lhs ps - evalExp rhs ps
  Mul -> evalExp lhs ps * evalExp rhs ps
  Div -> evalExp lhs ps `div` evalExp rhs ps
  Less -> if evalExp lhs ps < evalExp rhs ps then 1 else 0
  Equal -> if evalExp lhs ps == evalExp rhs ps then 1 else 0
--
evalExp (Deref expr) ps@(ms, _) =
  let addr = evalExp expr ps
   in case lookup addr ms of
        Just val -> val
        Nothing -> error ("Memory at address " ++ show addr ++ " has no value.")
evalExp (MulDeref derefCount innerExp) ps =
  let innerExpVal = evalExp (Deref innerExp) ps
   in if derefCount == 1
        then innerExpVal
        else evalExp (MulDeref (derefCount - 1) (Lit innerExpVal)) ps

allocMem :: MemoryState -> Int
allocMem ms = case ms of
  [] -> 10
  _ -> maximum (map fst ms) + 10

allocMem1 :: MemoryState -> Int -> Int
allocMem1 ms newAddr = case ms of
  [] -> max newAddr 0 + 10
  _ -> max newAddr (maximum (map fst ms)) + 10

countDerefs :: Expr -> Int -> (Expr, Int)
countDerefs (Deref expr) curCount = countDerefs expr (curCount + 1)
countDerefs expr curCount = (expr, curCount)

evalDerefAssign :: Int -> Int -> Int -> MemoryState -> MemoryState
evalDerefAssign derefCount firstAddr rhsVal ms =
  let (finalAddr, updatedMem) =
        if derefCount == 1
          then (firstAddr, ms)
          else
            foldl
              ( \(prevAddr, mem) _ ->
                  let newAddr = allocMem1 mem prevAddr
                   in (newAddr, updateMem prevAddr newAddr mem)
              )
              (firstAddr, ms)
              [1 .. (derefCount - 1)]
   in updateMem finalAddr rhsVal updatedMem

data Infinitable = NegInf | Reg Int | PosInf deriving (Show, Eq, Ord)

-- Execute statement
runStatement :: Statement -> ProgramState -> LabelDict -> (ProgramState, Maybe Infinitable)
runStatement (Assignment (Var name) rhsExp) ps@(ms, vs) _ =
  let rhsVal = evalExp rhsExp ps
      addr = case lookup name vs of
        Just existingAddr -> existingAddr
        Nothing -> allocMem ms
      updatedPs = (updateMem addr rhsVal ms, updateVars name addr vs)
   in (updatedPs, Nothing)
--
runStatement (Assignment (Deref derefExp) rhsExp) ps@(ms, vs) _ =
  let rhsVal = evalExp rhsExp ps
      (innerExp, derefCount) = countDerefs derefExp 1
      firstAddr = evalExp innerExp ps

      finalMem = evalDerefAssign derefCount firstAddr rhsVal ms
   in ((finalMem, vs), Nothing)
--
runStatement (Assignment (MulDeref derefCount innerExp) rhsExp) ps@(ms, vs) _ =
  let rhsVal = evalExp rhsExp ps
      firstAddr = evalExp innerExp ps

      finalMem = evalDerefAssign derefCount firstAddr rhsVal ms
   in ((finalMem, vs), Nothing)
--
runStatement (Send valExp addrExp) ps@(ms, vs) _ =
  let rhsVal = evalExp valExp ps
      addr = evalExp addrExp ps
   in ((updateMem addr rhsVal ms, vs), Nothing)
--
runStatement Stop ps _ = (ps, Just PosInf)
--
runStatement (Jump label) ps labelDict =
  case lookup label labelDict of
    Just lineNum -> (ps, Just (Reg lineNum))
    Nothing -> error ("Label " ++ show label ++ " not defined.")
--
runStatement (Conditional ifExp thenSt elseSt) ps ld =
  let ifExpVal = evalExp ifExp ps
   in if ifExpVal /= 0
        then runStatement thenSt ps ld
        else runStatement elseSt ps ld
--
runStatement _ ps _ = (ps, Nothing)

updateVars :: String -> Int -> VarState -> VarState
updateVars name addr vs = sortBy (compare `on` fst) ((name, addr) : [(nm, ad) | (nm, ad) <- vs, nm /= name])

updateMem :: Int -> Int -> MemoryState -> MemoryState
updateMem addr rhsVal ms = sortBy (compare `on` fst) ((addr, rhsVal) : [(ad, val) | (ad, val) <- ms, ad /= addr])

runProgLine' :: ProgLine -> ProgramState -> LabelDict -> (ProgramState, Maybe Infinitable)
runProgLine' (ProgLine _ stmts) ps labelDict =
  let stmtCount = length stmts
      (_, updatedPs, jumpToLn) =
        until
          (\(stmtIndex, _, jumpToLine) -> isJust jumpToLine || stmtIndex >= stmtCount)
          ( \(stmtIndex, curProgState, _) ->
              let stmt = stmts !! stmtIndex
                  (newProgState, jumpToLine) = runStatement stmt curProgState labelDict
               in (stmtIndex + 1, newProgState, jumpToLine)
          )
          (0, ps, Nothing)
   in (updatedPs, jumpToLn)

runProgLine :: ProgLine -> ProgramState -> LabelDict -> ProgramState
runProgLine pl ps labelDict = fst (runProgLine' pl ps labelDict)

runProgram :: Program -> ProgramState -> ProgramState
runProgram (Program pLines) ps =
  let labels = map (\(ProgLine lbl _) -> fromMaybe "" lbl) pLines
      labelDict = zip labels [0 ..]
      pLineCount = length pLines
      (_, finalState) =
        until
          (\(ln, _) -> ln >= Reg pLineCount)
          (runProgramStep pLines labelDict)
          (Reg 0, ps)
   in finalState

runProgramStep :: [ProgLine] -> LabelDict -> (Infinitable, ProgramState) -> (Infinitable, ProgramState)
runProgramStep pLines labelDict (Reg lineIndex, curProgState) =
  let pLine = pLines !! lineIndex
      (newProgState, jumpToLine) = runProgLine' pLine curProgState labelDict
      nextLine = fromMaybe (Reg (lineIndex + 1)) jumpToLine
   in (nextLine, newProgState)
runProgramStep _ _ _ = error "Wrong line index"

scanUntil :: (a -> Bool) -> (a -> a) -> a -> [a]
scanUntil p f initial = takeWhile (not . p) $ iterate f initial

-- prog :: Program
-- prog =
--   Program
--     [ ProgLine (Just "1") [Assignment (Var "h") (Lit 20)],
--       ProgLine (Just "2") [Assignment (MulDeref 3 (Var "h")) (Lit 0)],
--       ProgLine (Just "3") [Assignment (Deref (BinOpApp Add (Var "h") (Lit 1))) (Lit 1)],
--       ProgLine (Just "4") [Assignment (Deref (BinOpApp Add (Deref (Var "h")) (Lit 1))) (Lit 2)],
--       ProgLine (Just "5") [Assignment (Deref (BinOpApp Add (MulDeref 2 (Var "h")) (Lit 1))) (Lit 3)],
--       ProgLine Nothing [Stop],
--       ProgLine (Just "6") [Assignment (Var "res") (Deref (BinOpApp Add (MulDeref 2 (Var "h")) (Lit 1)))]
--     ]

prog :: Program
prog =
  Program
    [ ProgLine Nothing [Assignment (Var "a") (Lit 20)],
      ProgLine Nothing [Assignment (Deref (Var "a")) (Lit 2)],
      ProgLine (Just "lab1") [],
      ProgLine
        Nothing
        [ Conditional
            (BinOpApp Less (Deref (Var "a")) (Lit 30))
            Stop
            (Assignment (Deref (Var "a")) (BinOpApp Mul (Deref (Var "a")) (Lit 2)))
        ],
      ProgLine Nothing [Jump "lab1"]
    ]

res :: ProgramState
res = runProgram prog ([], [])