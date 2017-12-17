module SmallStepSemantics where

import Text.Printf (printf)

data Expr = 
  Number Int | Add Expr Expr | Mult Expr Expr deriving Eq

showBinOp op n1 n2 = printf "%s%c%s"(show n1) op (show n2)

instance Show Expr where
  show e = case e of
    Number n -> show n
    Add n1 n2 -> showBinOp '+' n1 n2
    Mult n1 n2 -> showBinOp '*' n1 n2

isReducible :: Expr -> Bool
isReducible e = case e of
  Number _ -> False
  _ -> True

value :: Expr -> Int
value e = case e of
  Number n -> n
  _ -> error "not fully reduced"

reduce :: Expr -> Expr
reduce e = case e of
  Add e1 e2 -> reduceOp Add e1 e2
  Mult e1 e2 -> reduceOp Mult e1 e2
  n -> n

reduceOp :: (Expr -> Expr -> Expr) -> Expr -> Expr -> Expr
reduceOp op e1 e2 = case (isReducible e1, isReducible e2) of
  (True, _) -> op (reduce e1) e2
  (False, True) -> op e1 (reduce e2)
  _ -> Number (value e1 + value e2)