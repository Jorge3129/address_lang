module Parse where

import AST
import Control.Monad (void)
import Text.Parsec (endOfLine)
import Text.ParserCombinators.Parsec

-- UTILS
number :: Parser Int
number = do
  s <- option "" (string "-")
  ds <- many1 digit
  return $ read (s ++ ds)

infOp :: String -> (a -> a -> a) -> Parser (a -> a -> a)
infOp x f = string x >> return f

wspaces :: Parser ()
wspaces = void $ many (oneOf " \t")

lexem :: Parser a -> Parser a
lexem p = do a <- p; wspaces; return a

reserved :: String -> Parser ()
reserved s = string s >> wspaces

parens :: Parser a -> Parser a
parens p = do reserved "("; n <- lexem p; reserved ")"; return n

constant :: Parser Expr
constant = Lit <$> lexem number

identifierString :: Parser String
identifierString = do
  let latinAlf = ['A' .. 'Z'] ++ ['a' .. 'z']
      nums = ['0' .. '9']
      firstLetter = latinAlf
      subseqLetter = latinAlf ++ nums ++ "_$#@"
  st <- oneOf firstLetter
  nx <- many (oneOf subseqLetter)
  return (st : nx)

identifier :: Parser Expr
identifier = Var <$> lexem identifierString

-- ARITHMETIC
addopA, mulopA, cmpOpA :: Parser (Expr -> Expr -> Expr)
addopA = infOp "+" (BinOpApp Add) <|> infOp "-" (BinOpApp Sub)
mulopA = infOp "*" (BinOpApp Mul) <|> infOp "/" (BinOpApp Div)
cmpOpA =
  infOp "==" (BinOpApp Equal)
    <|> infOp "<" (BinOpApp Less)

deref :: Parser Expr
deref = do string "'"; Deref <$> parens expression

deref' :: Parser Expr -> Parser Expr
deref' p = do string "'"; Deref <$> p

mulDeref :: Parser Expr
mulDeref = do
  string "`"
  n <- lexem number
  string "`"
  MulDeref n <$> parens expression

mulDeref' :: Parser Expr -> Parser Expr
mulDeref' p = do
  string "`"
  n <- lexem number
  string "`"
  MulDeref n <$> parens p

-- EXPRESSIONS
expression :: Parser Expr
expression = chainl1 relExpr (lexem cmpOpA)

relExpr :: Parser Expr
relExpr = chainl1 mulExpr (lexem addopA)

mulExpr :: Parser Expr
mulExpr = chainl1 unaryExpr (lexem mulopA)

unaryExpr :: Parser Expr
unaryExpr = deref' unaryExpr <|> mulDeref' unaryExpr <|> primaryExpr

primaryExpr :: Parser Expr
primaryExpr = constant <|> identifier <|> parens expression

-- STATEMENTS
statement :: Parser Statement
statement = stopSt <|> try printSt <|> try condSt <|> try assignSt <|> jumpSt

printSt :: Parser Statement
printSt = do
  lexem (string "print")
  Print <$> expression

assignLhs :: Parser Expr
assignLhs = lexem $ identifier <|> deref' unaryExpr <|> mulDeref' unaryExpr

assignSt :: Parser Statement
assignSt = do
  lhs <- assignLhs
  reserved "="
  Assignment lhs <$> expression

jumpSt :: Parser Statement
jumpSt = Jump <$> lexem identifierString

stopSt :: Parser Statement
stopSt = do
  reserved "!"
  return Stop

condSt :: Parser Statement
condSt = do
  reserved "P"
  reserved "{"
  ifExp <- expression
  reserved "}"
  reserved "("
  thenSt <- statement
  reserved "|"
  elseSt <- statement
  reserved ")"
  return (Conditional ifExp thenSt elseSt)

parseLabel :: Parser String
parseLabel = do
  label <- lexem identifierString
  reserved "..."
  return label

parseLine :: Parser ProgLine
parseLine = do
  mbLabel <- optionMaybe (try parseLabel)
  stmts <- statement `sepBy` reserved ";"
  return (ProgLine mbLabel stmts)

parseProg :: Parser Program
parseProg = do
  pLines <- parseLine `sepBy` endOfLine
  return (Program pLines)

full :: Parser a -> Parser a
full p = do _ <- spaces; v <- p; eof; return v

parseOrThrow :: Parser a -> String -> a
parseOrThrow p str =
  case parse (full p) "" str of
    (Left x) -> error (show x)
    (Right x) -> x
