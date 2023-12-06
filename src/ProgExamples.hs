module ProgExamples where

import AST

prog1 :: Program
prog1 =
  Program
    [ ProgLine (Just "1") [Assignment (Var "h") (Lit 20)],
      ProgLine (Just "2") [Assignment (MulDeref 3 (Var "h")) (Lit 0)],
      ProgLine (Just "3") [Assignment (Deref (BinOpApp Add (Var "h") (Lit 1))) (Lit 1)],
      ProgLine (Just "4") [Assignment (Deref (BinOpApp Add (Deref (Var "h")) (Lit 1))) (Lit 2)],
      ProgLine (Just "5") [Assignment (Deref (BinOpApp Add (MulDeref 2 (Var "h")) (Lit 1))) (Lit 3)],
      ProgLine Nothing [Stop],
      ProgLine (Just "6") [Assignment (Var "res") (Deref (BinOpApp Add (MulDeref 2 (Var "h")) (Lit 1)))]
    ]

prog2 :: Program
prog2 =
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