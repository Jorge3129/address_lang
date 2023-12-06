
module Test where

import AST
import Lang
import Parse

-- bar :: Expr
-- bar = parseOrThrow exprA "'(a) + '('(b)) * `3`(h)"

-- foo :: Int
-- foo = evalExp (bar) ([],[])

-- bar :: Statement
-- bar = parseOrThrow statement "'(c) = '(a) + '('(b)) * `3`(h)"

-- bar :: ProgLine
-- bar = parseOrThrow parseLine "P {'(a) == 5} ( lab1 | '(a) = '(a) + 1)"

-- bar :: ProgLine
-- bar = parseOrThrow parseLine "lab1 ... P {'(a) == 5} ( lab1 | '(a) = '(a) + 1); '(foo) = '(bar)"


progText :: String
progText = "a = 20\n"
    ++ "'(a) = 2\n"
    ++ "lab1 ... \n"
    ++ "P { '(a) < 30 } (! | '(a) = '(a) * 2)\n"
    ++ "lab1"

progRes = parseOrThrow parseProg progText