module ExprT where

data ExprT = Const Integer
           | Add ExprT ExprT
           | Mul ExprT ExprT
  deriving (Show, Eq)