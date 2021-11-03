{-# LANGUAGE FlexibleInstances #-}

import qualified Data.Map as M
import Data.Maybe

class Expr a where
    con :: Integer -> a
    add :: a -> a -> a
    mul :: a -> a -> a

class HasVars a where
    var :: String -> a

data VarExprT = Const Integer
              | Var String
              | Add VarExprT VarExprT
              | Mul VarExprT VarExprT
                deriving (Show, Eq)

instance Expr VarExprT where
    con n = Const n
    add = Add
    mul = Mul

instance HasVars VarExprT where
    var = Var


-- by 오현석
-- use lambda function and isNothing
-- we can rewrite above instance as follows.

type VarExprT' = M.Map String Integer -> Maybe Integer

instance HasVars VarExprT' where
    var = M.lookup

instance Expr VarExprT' where
    con n = \_ -> Just n
    add f g = \m -> if isNothing (f m) || isNothing (g m)
                    then Nothing
                    else Just(fromJust (f m) + fromJust (g m))

    mul f g = \m -> if isNothing (f m) || isNothing (g m)
                    then Nothing
                    else Just(fromJust (f m) * fromJust (g m))

withVars :: [(String, Integer)] -> VarExprT' -> Maybe Integer
withVars vs exp = exp $ M.fromList vs
