module Parse where

import Text.ParserCombinators.Parsec
import AST

-- UTILS
number :: Parser Int
number = do 
  s <- option "" (string "-")
  ds <- many1 digit
  return $ read (s ++ ds) 

infOp :: String -> (a -> a -> a) -> Parser (a -> a -> a)
infOp x f = string x >> return f

lexem :: Parser a -> Parser a
lexem p = do { a <- p; spaces; return a}

reserved :: String -> Parser ()
reserved s = do { _ <- string s; spaces }

parens :: Parser a -> Parser a
parens p = do {reserved "("; n <- lexem p; reserved ")"; return n} 

int :: Parser Expr
int = do {n <- lexem number; return (Lit n)}

latinAlf :: String
latinAlf = ['A'..'Z'] ++ ['a'..'z']

nums :: String
nums = ['0'..'9']

firstLetter :: String
firstLetter = latinAlf

subseqLetter :: String
subseqLetter = latinAlf ++ nums ++ "_$#@"

idf :: Parser String
idf = do 
  st <- oneOf firstLetter
  nx <- many (oneOf subseqLetter)
  return (st:nx)

idp :: Parser Expr
idp = do
  st <- lexem idf
  return (Var st)

-- ARITHMETIC
addopA, mulopA :: Parser (Expr -> Expr -> Expr)
addopA = infOp "+" (BinOpApp Add) <|> infOp "-" (BinOpApp Sub)
mulopA = infOp "*"(BinOpApp Mul) <|> infOp "/" (BinOpApp Div)
cmpOpA = infOp "==" (BinOpApp Equal) 
          <|> infOp "<" (BinOpApp Less)

deref :: Parser Expr
deref = do
    string "'"
    innerExp <- parens exprA
    return (Deref innerExp)

mulDeref :: Parser Expr
mulDeref = do
    string "`"
    -- powerExp <- parens exprA
    n <- lexem number
    string "`"
    innerExp <- parens exprA
    return (MulDeref n innerExp)

-- EXPRESSIONS
exprA, termA, factorA :: Parser Expr
factorA = int <|> idp <|> deref <|> mulDeref <|> parens exprA
termA = chainl1 factorA (lexem mulopA)
eqTermA = chainl1 termA (lexem addopA)
exprA = chainl1 eqTermA (lexem cmpOpA)

-- STATEMENTS
-- semicolon :: Parser a -> Parser a
-- semicolon p = do { v <- lexem p; reserved ";"; return v}

statement :: Parser Statement
statement = stopSt <|> try condSt <|> try assignSt <|> jumpSt

assignSt :: Parser Statement
assignSt = do
  lhs <- lexem eqTermA
  reserved "="
  rhs <- exprA
  return (Assignment lhs rhs)

jumpSt :: Parser Statement
jumpSt = do
  label <- lexem idf
  return (Jump label)

stopSt :: Parser Statement
stopSt = do
  reserved "!"
  return (Stop)

condSt :: Parser Statement
condSt = do
  reserved "P"
  reserved "{"
  ifExp <- exprA
  reserved "}"
  reserved "(";
  thenSt <- statement
  reserved "|";
  elseSt <- statement
  reserved ")";
  return (Conditional ifExp thenSt elseSt)

parseLabel :: Parser String
parseLabel = do
  label <- lexem idf
  reserved "..."
  return label

parseLine :: Parser ProgLine
parseLine = do
  mbLabel <- optionMaybe (try parseLabel)
  stmts <- statement `sepBy` reserved ";"
  return (ProgLine mbLabel stmts)

parseProg :: Parser Program
parseProg = do
  pLines <- parseLine `sepBy` reserved "\n"
  return (Program pLines)

-- whileSt :: Parser Stmt
-- whileSt = do
--   reserved "while"
--   cnd <- parens exprA
--   While cnd <$> statement

-- callSt :: Parser Stmt
-- callSt = do
--   reserved "call"
--   nm <- lexem idf
--   Call nm <$> callArgs

-- blockSt :: Parser Stmt
-- blockSt = do
--   reserved "{"
--   vars <- many (try (semicolon varDef))
--   stmts <- sepBy statement (reserved ";")
--   reserved "}"
--   return (Block vars stmts)

-- -- FUNCTION DEFINITION
-- funcDef :: Parser FunDef
-- funcDef = do
--   reserved "func"
--   nm <- lexem idf
--   vars <- parens (commaSep varDef)
--   reserved "="
--   ex <- exprA
--   return (nm, (vars, ex))

-- -- PROCEDURE DEFINITION
-- procDef :: Parser ProcDef
-- procDef = do
--   reserved "proc"
--   nm <- lexem idf
--   vars <- parens (commaSepOpt varDef)
--   sm <- statement
--   return (nm, (vars, sm))



-- prog :: Parser Program
-- prog = do
--   vars <- many (try (semicolon varDef))
--   fns <- many funcDef
--   procs <- many procDef
--   return (vars, fns, procs)

full :: Parser a -> Parser a
full p = do { _ <- spaces; v <- p; eof; return v}

parseOrThrow :: Parser a -> String -> a
parseOrThrow p str = 
  case parse (full p) "" str of
    (Left x) -> error (show x)
    (Right x) -> x

-- parseOrThrow funcDef dfSumA == sumA
-- parseOrThrow funcDef dfFib == fib
-- parseOrThrow procDef dpGAdd == gAdd
-- parseOrThrow procDef dpSumA1 == sumA1
-- parseOrThrow prog dPr1 == pr1
-- parseOrThrow prog dPr2 == pr2