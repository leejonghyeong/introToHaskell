{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

import Parser
import StackVM

-- Exercise 5
-- create an instance of Expr type class for Program
-- For example, for any arithmetic expression exp :: Expr a => a,
-- stackVM exp == Right [IVal exp] << 약간 abuse한듯

-- Finally create compile function
-- compile :: String -> Maybe Program

class Expr a where
    con :: Integer -> a
    add :: a -> a -> a
    mul :: a -> a -> a

-- stackVal (add (con n) (con m)) 
-- == execute [] (add [PushI n] [PushI m])
-- == Right [IVal n+m]
-- == execute [] ([PushI n, PushI m, Add]) 

instance Expr Program where
    con n = [PushI n]
    add x y = x ++ y ++ [Add]
    mul x y = x ++ y ++ [Mul]

compile :: String -> Maybe Program
compile = parseExp con add mul
