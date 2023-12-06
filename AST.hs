{-# OPTIONS_GHC -Wall #-}

module AST where

data BinOp = Add | Sub | Mul | Div | Less | Equal deriving (Eq)

instance Show BinOp where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"
  show Less = "<"
  show Equal = "=="

data Expr
  = Lit Int
  | Var String
  | BinOpApp BinOp Expr Expr
  | Deref Expr
  | MulDeref Int Expr
  deriving (Eq)

instance Show Expr where
  show (Lit val) = show val
  show (Var name) = name
  show (BinOpApp op a b) = show a ++ " " ++ show op ++ " " ++ show b
  show (Deref expr) = "'(" ++ show expr ++ ")"
  show (MulDeref n expr) = "`" ++ show n ++ "`(" ++ show expr ++ ")"

data Statement
  = Assignment Expr Expr
  | Send Expr Expr
  | Exchange Expr Expr
  | Conditional Expr Statement Statement
  | SubprogramCall [Expr] String (Maybe String)
  | Jump String
  | Stop

instance Show Statement where
  show (Assignment ex1 ex2) = show ex1 ++ " = " ++ show ex2
  show (Send ex1 ex2) = show ex1 ++ " => " ++ show ex2
  show (Exchange ex1 ex2) = show ex1 ++ " <=> " ++ show ex2
  show (SubprogramCall args name ret) =
    "П { "
      ++ foldl (\acc arg -> acc ++ (if acc == "" then "" else ", ") ++ show arg) "" args
      ++ " } "
      ++ name
      ++ maybe "" (" " ++) ret
  show (Conditional ifExp thenSt elseSt) =
    "P { "
      ++ show ifExp
      ++ " } ("
      ++ show thenSt
      ++ " | "
      ++ show elseSt
      ++ ")"
  show Stop = "!"
  show (Jump label) = label

data ProgLine = ProgLine (Maybe String) [Statement]

instance Show ProgLine where
  show (ProgLine maybeLabel stmts) =
    maybe "" (++ " ... ") maybeLabel
      ++ foldl (\acc stmt -> acc ++ (if acc == "" then "" else "; ") ++ show stmt) "" stmts

newtype Program = Program [ProgLine]

instance Show Program where
  show (Program pLines) =
    foldl (\acc pLine -> acc ++ (if acc == "" then "" else "\n") ++ show pLine) "" pLines

st1 :: Statement
st1 = Assignment (Var "a") (Lit 1)

st2 :: Statement
st2 = SubprogramCall [Var "a", Lit 2] "bar" (Just "foo")

st3 :: Statement
st3 = SubprogramCall [Var "a", Lit 2] "bar" Nothing

st4 :: Statement
st4 =
  Conditional
    (Var "d")
    ( Conditional
        (Var "b")
        (Assignment (Deref (Deref (Var "a"))) (Lit 1))
        (Send (MulDeref 2 (Var "b")) (Var "c"))
    )
    (Exchange (Var "e") (Var "f"))