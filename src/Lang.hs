{-# OPTIONS_GHC -Wall #-}

module Lang where

import AST
import Data.Function (on)
import Data.List (sortBy)
import Data.Maybe (fromMaybe, isJust)
import MyUtils

type MemoryState = [(Int, Int)]

type VarState = [(String, Int)]

type ProgramState = (MemoryState, VarState)

type LabelDict = [(String, Int)]

boolToInt :: Bool -> Int
boolToInt True = 1
boolToInt False = 0

evalExp :: Expr -> ProgramState -> Int
evalExp (Lit val) _ = val
--
evalExp (Var name) (ms, vs) = case lookup name vs of
  Just addr -> case lookup addr ms of
    Just val -> val
    Nothing -> error ("Variable " ++ name ++ " has no value in memory.")
  Nothing -> error ("Variable " ++ name ++ " is not defined.")
--
evalExp (BinOpApp op lhs rhs) ps = binOp $ case op of
  Add -> (+)
  Sub -> (-)
  Mul -> (*)
  Div -> div
  Less -> (\x y -> boolToInt (x < y))
  Equal -> (\x y -> boolToInt (x == y))
  where
    binOp :: (Int -> Int -> a) -> a
    binOp f = f (evalExp lhs ps) (evalExp rhs ps)
--
evalExp (Deref expr) ps@(ms, _) =
  let addr = evalExp expr ps
   in case lookup addr ms of
        Just val -> val
        Nothing -> error ("Memory at address " ++ show addr ++ " has no value.")
--
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

-- Execute statement
runStatement :: Statement -> ProgramState -> LabelDict -> IO (ProgramState, Maybe Infinitable)
runStatement (Assignment (Var name) rhsExp) ps@(ms, vs) _ =
  let rhsVal = evalExp rhsExp ps
      addr = case lookup name vs of
        Just existingAddr -> existingAddr
        Nothing -> allocMem ms
      updatedPs = (updateMem addr rhsVal ms, updateVars name addr vs)
   in pure (updatedPs, Nothing)
--
runStatement (Assignment (Deref derefExp) rhsExp) ps@(ms, vs) _ =
  let rhsVal = evalExp rhsExp ps
      (innerExp, derefCount) = countDerefs derefExp 1
      firstAddr = evalExp innerExp ps

      finalMem = evalDerefAssign derefCount firstAddr rhsVal ms
   in pure ((finalMem, vs), Nothing)
--
runStatement (Assignment (MulDeref derefCount innerExp) rhsExp) ps@(ms, vs) _ =
  let rhsVal = evalExp rhsExp ps
      firstAddr = evalExp innerExp ps

      finalMem = evalDerefAssign derefCount firstAddr rhsVal ms
   in pure ((finalMem, vs), Nothing)
--
runStatement (Send valExp addrExp) ps@(ms, vs) _ =
  let rhsVal = evalExp valExp ps
      addr = evalExp addrExp ps
   in pure ((updateMem addr rhsVal ms, vs), Nothing)
--
runStatement Stop ps _ = pure (ps, Just PosInf)
--
runStatement (Jump label) ps labelDict =
  case lookup label labelDict of
    Just lineNum -> pure (ps, Just (Reg lineNum))
    Nothing -> error ("Label " ++ show label ++ " not defined.")
--
runStatement (Conditional ifExp thenSt elseSt) ps ld =
  let ifExpVal = evalExp ifExp ps
   in if ifExpVal /= 0
        then runStatement thenSt ps ld
        else runStatement elseSt ps ld
--
runStatement (Print ex) ps _ = do
  print $ evalExp ex ps
  return (ps, Nothing)
--
runStatement _ ps _ = pure (ps, Nothing)

updateVars :: String -> Int -> VarState -> VarState
updateVars name addr vs = sortBy (compare `on` fst) ((name, addr) : [(nm, ad) | (nm, ad) <- vs, nm /= name])

updateMem :: Int -> Int -> MemoryState -> MemoryState
updateMem addr rhsVal ms = sortBy (compare `on` fst) ((addr, rhsVal) : [(ad, val) | (ad, val) <- ms, ad /= addr])

runProgLine' :: ProgLine -> ProgramState -> LabelDict -> IO (ProgramState, Maybe Infinitable)
runProgLine' (ProgLine _ stmts) ps labelDict = do
  let stmtCount = length stmts
  (_, updatedPs, jumpToLn) <-
    untilM
      (\(stmtIndex, _, jumpToLine) -> isJust jumpToLine || stmtIndex >= stmtCount)
      ( \(stmtIndex, curProgState, _) -> do
          let stmt = stmts !! stmtIndex
          (newProgState, jumpToLine) <- runStatement stmt curProgState labelDict
          return (stmtIndex + 1, newProgState, jumpToLine)
      )
      (0, ps, Nothing)
  return (updatedPs, jumpToLn)

runProgLine :: ProgLine -> ProgramState -> LabelDict -> IO ProgramState
runProgLine pl ps labelDict = fmap fst (runProgLine' pl ps labelDict)

runProgram :: Program -> ProgramState -> IO ProgramState
runProgram (Program pLines) ps = do
  let labels = map (\(ProgLine lbl _) -> fromMaybe "" lbl) pLines
      labelDict = zip labels [0 ..]
      pLineCount = length pLines
  (_, finalState) <-
    untilM
      (\(ln, _) -> ln >= Reg pLineCount)
      (runProgramStep pLines labelDict)
      (Reg 0, ps)
  return finalState

runProgramStep :: [ProgLine] -> LabelDict -> (Infinitable, ProgramState) -> IO (Infinitable, ProgramState)
runProgramStep pLines labelDict (Reg lineIndex, curProgState) = do
  let pLine = pLines !! lineIndex
  (newProgState, jumpToLine) <- runProgLine' pLine curProgState labelDict
  let nextLine = fromMaybe (Reg (lineIndex + 1)) jumpToLine
  return (nextLine, newProgState)
runProgramStep _ _ _ = error "Wrong line index"