{-# OPTIONS_GHC -Wall #-}

module Lang where

import AST
import Data.Map (fromList, insert, keys, lookup, null)
import Data.Maybe (fromMaybe, isJust)
import MyUtils
import ProgState

evalExp :: Expr -> ProgState -> Int
evalExp (Lit val) _ = val
--
evalExp (Var name) (ProgState ms vs) = case Data.Map.lookup name vs of
  Just addr -> case Data.Map.lookup addr ms of
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
evalExp (Deref expr) ps@(ProgState ms _) =
  let addr = evalExp expr ps
   in case Data.Map.lookup addr ms of
        Just val -> val
        Nothing -> error ("Memory at address " ++ show addr ++ " has no value.")
--
evalExp (MulDeref derefCount innerExp) ps =
  case derefCount of
    1 -> innerExpVal
    cnt -> evalExp (MulDeref (cnt - 1) (Lit innerExpVal)) ps
  where
    innerExpVal = evalExp (Deref innerExp) ps

allocMem :: MemoryState -> Int
allocMem ms
  | Data.Map.null ms = 10
  | otherwise = maximum (keys ms) + 10

allocMem1 :: MemoryState -> Int -> Int
allocMem1 ms newAddr
  | Data.Map.null ms = max newAddr 0 + 10
  | otherwise = max newAddr (maximum (keys ms)) + 10

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
runStatement :: Statement -> ProgState -> LabelDict -> IO (ProgState, Maybe Infinitable)
runStatement (Assignment (Var name) rhsExp) ps@(ProgState ms vs) _ =
  let rhsVal = evalExp rhsExp ps
      addr = fromMaybe (allocMem ms) (Data.Map.lookup name vs)
      updatedPs = ProgState (updateMem addr rhsVal ms) (updateVars name addr vs)
   in pure (updatedPs, Nothing)
--
runStatement (Assignment (Deref derefExp) rhsExp) ps@(ProgState ms vs) _ =
  let rhsVal = evalExp rhsExp ps
      (innerExp, derefCount) = countDerefs derefExp 1
      firstAddr = evalExp innerExp ps
      finalMem = evalDerefAssign derefCount firstAddr rhsVal ms
   in pure (ProgState finalMem vs, Nothing)
--
runStatement (Assignment (MulDeref derefCount innerExp) rhsExp) ps@(ProgState ms vs) _ =
  let rhsVal = evalExp rhsExp ps
      firstAddr = evalExp innerExp ps
      finalMem = evalDerefAssign derefCount firstAddr rhsVal ms
   in pure (ProgState finalMem vs, Nothing)
--
runStatement (Send valExp addrExp) ps@(ProgState ms vs) _ =
  let rhsVal = evalExp valExp ps
      addr = evalExp addrExp ps
   in pure (ProgState (updateMem addr rhsVal ms) vs, Nothing)
--
runStatement Stop ps _ = pure (ps, Just PosInf)
--
runStatement (Jump label) ps labelDict =
  case Data.Map.lookup label labelDict of
    Just lineNum -> pure (ps, Just (Reg lineNum))
    Nothing -> error ("Label " ++ show label ++ " not defined.")
--
runStatement (Conditional ifExp thenSt elseSt) ps ld = runStatement st ps ld
  where
    st = case evalExp ifExp ps of
      0 -> elseSt
      _ -> thenSt
--
runStatement (Print ex) ps _ = do
  print $ evalExp ex ps
  return (ps, Nothing)
--
runStatement _ ps _ = pure (ps, Nothing)

updateVars :: String -> Int -> VarState -> VarState
updateVars = insert

updateMem :: Int -> Int -> MemoryState -> MemoryState
updateMem = insert

runProgLine' :: ProgLine -> ProgState -> LabelDict -> IO (ProgState, Maybe Infinitable)
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

runProgLine :: ProgLine -> ProgState -> LabelDict -> IO ProgState
runProgLine pl ps labelDict = fmap fst (runProgLine' pl ps labelDict)

runProgram :: Program -> ProgState -> IO ProgState
runProgram (Program pLines) ps = do
  let labels = map (\(ProgLine lbl _) -> fromMaybe "" lbl) pLines
      labelDict = fromList $ zip labels [0 ..]
      pLineCount = length pLines
  (_, finalState) <-
    untilM
      (\(ln, _) -> ln >= Reg pLineCount)
      (runProgramStep pLines labelDict)
      (Reg 0, ps)
  return finalState

runProgramStep :: [ProgLine] -> LabelDict -> (Infinitable, ProgState) -> IO (Infinitable, ProgState)
runProgramStep pLines labelDict (Reg lineIndex, curProgState) = do
  let pLine = pLines !! lineIndex
  (newProgState, jumpToLine) <- runProgLine' pLine curProgState labelDict
  let nextLine = fromMaybe (Reg (lineIndex + 1)) jumpToLine
  return (nextLine, newProgState)
runProgramStep _ _ _ = error "Wrong line index"