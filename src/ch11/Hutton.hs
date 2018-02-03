data Expr
    = Lit Integer
    | Add Expr Expr

eval :: Expr -> Integer
eval (Lit x) = x
eval (Add e e') = (+) (eval e) (eval e')

printExpr :: Expr -> String
printExpr (Lit x) = show x
printExpr (Add e e') = (printExpr e) ++ " + " ++ (printExpr e')
