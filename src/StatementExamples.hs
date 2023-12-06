import AST

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