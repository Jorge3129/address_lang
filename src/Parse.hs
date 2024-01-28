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

nil :: Parser Expr
nil = reserved "Nil" >> return Nil

-- ARITHMETIC
addopA, mulopA, cmpOpA :: Parser (Expr -> Expr -> Expr)
addopA = infOp "+" (BinOpApp Add) <|> infOp "-" (BinOpApp Sub)
mulopA = infOp "*" (BinOpApp Mul) <|> infOp "/" (BinOpApp Div)
cmpOpA =
  infOp "==" (BinOpApp Equal)
    <|> infOp "<" (BinOpApp Less)

deref' :: Parser Expr -> Parser Expr
deref' p = do string "'"; Deref <$> p

mulDeref' :: Parser Expr -> Parser Expr
mulDeref' p = do
  string "`"
  n <- unaryExpr
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
primaryExpr = try nil <|> constant <|> identifier <|> parens expression

-- STATEMENTS
statement :: Parser Statement
statement =
  stopSt
    <|> try builtinFuncSt
    <|> try subProgCallSt
    <|> try condSt
    <|> try sendSt
    <|> try assignSt
    <|> jumpSt

builtinFuncSt :: Parser Statement
builtinFuncSt = try printListSt <|> try printSt

printSt :: Parser Statement
printSt = do
  lexem (string "print")
  ex <- expression
  return $ BuiltinFunc "print" [ex]

printListSt :: Parser Statement
printListSt = do
  lexem (string "printList")
  ex <- expression
  return $ BuiltinFunc "printList" [ex]

assignLhs :: Parser Expr
assignLhs = lexem $ identifier <|> deref' unaryExpr <|> mulDeref' unaryExpr

sendSt :: Parser Statement
sendSt = do
  lhs <- try expression
  reserved ".=>"
  Send lhs <$> assignLhs

assignSt :: Parser Statement
assignSt = do
  lhs <- assignLhs
  reserved "="
  Assignment lhs <$> expression

subProgCallSt :: Parser Statement
subProgCallSt = do
  reserved "Pg"
  name <- lexem identifierString
  reserved "{"
  args <- expression `sepBy` reserved ","
  reserved "}"
  return $ SubprogramCall name args Nothing

jumpSt :: Parser Statement
jumpSt = Jump <$> lexem identifierString

stopSt :: Parser Statement
stopSt = reserved "!" >> return Stop

condSt :: Parser Statement
condSt = do
  reserved "P" >> reserved "{"
  ifExp <- expression
  reserved "}"
  reserved "("
  thenSt <- statement
  reserved "|"
  elseSt <- statement
  reserved ")"
  return (Conditional ifExp thenSt elseSt)

lineLabel :: Parser String
lineLabel = lexem identifierString <* reserved "..."

stmtListBy :: String -> Parser [Statement]
stmtListBy sep = statement `sepBy` reserved sep

parseLine :: Parser ProgLine
parseLine = do
  mbLabel <- optionMaybe (try lineLabel)
  ProgLine mbLabel <$> stmtListBy ";"

parseProg :: Parser Program
parseProg = Program <$> parseLine `sepBy` endOfLine

full :: Parser a -> Parser a
full p = do _ <- spaces; v <- p; eof; return v

parseOrThrow :: Parser a -> String -> a
parseOrThrow p str =
  case parse (full p) "" str of
    (Left x) -> error (show x)
    (Right x) -> x
