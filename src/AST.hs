{-# OPTIONS_GHC -Wall #-}

module AST where

import Data.List (intercalate)

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
  | MulDeref Expr Expr
  | Nil
  deriving (Eq)

instance Show Expr where
  show (Lit val) = show val
  show (Var name) = name
  show (BinOpApp op a b) = show a ++ " " ++ show op ++ " " ++ show b
  show (Deref expr) = "'(" ++ show expr ++ ")"
  show (MulDeref n expr) = "`" ++ show n ++ "`(" ++ show expr ++ ")"
  show Nil = "NIL"

data Statement
  = Assignment Expr Expr
  | Send Expr Expr
  | Exchange Expr Expr
  | Conditional Expr Statement Statement
  | SubprogramCall String [Expr] (Maybe String)
  | BuiltinFunc String [Expr]
  | Jump String
  | CompJump Expr
  | Stop

instance Show Statement where
  show (Assignment ex1 ex2) = show ex1 ++ " = " ++ show ex2
  show (Send ex1 ex2) = show ex1 ++ " .=> " ++ show ex2
  show (Exchange ex1 ex2) = show ex1 ++ " <=> " ++ show ex2
  show (SubprogramCall name args ret) =
    "Pg "
      ++ name
      ++ " { "
      ++ intercalate ", " (map show args)
      ++ " } "
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
  show (CompJump ex) = show ex
  show (Jump label) = label
  show (BuiltinFunc name args) = name ++ " " ++ intercalate " " (map show args)

data ProgLine = ProgLine (Maybe String) [Statement]

instance Show ProgLine where
  show (ProgLine maybeLabel stmts) =
    maybe "" (++ " ... ") maybeLabel
      ++ intercalate "; " (map show stmts)

newtype Program = Program [ProgLine]

instance Show Program where
  show (Program pLines) =
    foldl (\acc pLine -> acc ++ (if acc == "" then "" else "\n") ++ show pLine) "" pLines