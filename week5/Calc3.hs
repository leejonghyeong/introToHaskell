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

type VarExprT' = M.Map String Integer -> Maybe Integer

-- var :: k -> Map k v -> Maybe v
-- lookup :: k -> Map k v -> Maybe v 
instance HasVars VarExprT' where
    var = M.lookup

-- con :: Integer -> Map k v -> Maybe v
-- add :: (Map k v -> Maybe v) -> (Map k v -> Maybe v) -> (Map k v -> Maybe v)
-- add f g :: Map k v -> Maybe v
instance Expr VarExprT' where
    con n m = Just n
    add f g m = case (f m, g m) of
                (Just x, Just y) -> Just (x + y)
                _ -> Nothing
    mul f g m = case (f m, g m) of
                (Just x, Just y) -> Just (x * y)
                _ -> Nothing

withVars :: [(String, Integer)] -> VarExprT' -> Maybe Integer
withVars vs exp = exp $ M.fromList vs
