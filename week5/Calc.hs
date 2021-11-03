import ExprT
import Parser
--version1 of caculator
eval :: ExprT -> Integer
eval (Const n) = n
eval (Add x y) = eval x + eval y
eval (Mul x y) = eval x * eval y

--implement value-added function
evalStr :: String -> Maybe Integer
evalStr s = case parseExp Const Add Mul s of
            Nothing -> Nothing
            Just exprt -> Just (eval exprt)

-- using functor fmap
-- fmap f Nothing = Nothing
-- fmap f Just x = Just (f x)
evalStr' :: String -> Maybe Integer
evalStr' s = fmap eval (parseExp Const Add Mul s)

-- define type class Expr, abstract version of ExprT
class Expr a where
    con :: Integer -> a
    add :: a -> a -> a
    mul :: a -> a -> a

-- instance of the class Expr for the type ExprT
instance Expr ExprT where
    con = Const
    add = Add
    mul = Mul

-- purpose of reify fcn is to constrain the type of its argument to ExprT
-- by reify $ (...)
reify :: ExprT -> ExprT
reify = id


-- Introduce Integer, Bool, MinMax, Mod7 and specify their operations in Expr
instance Expr Integer where
    con = id
    add = (+)
    mul = (*)

instance Expr Bool where
    con n | n <= 0 = False
          | otherwise = True
    add = (||)
    mul = (&&)

newtype MinMax = MinMax Integer deriving(Eq, Show)
newtype Mod7 = Mod7 Integer deriving(Eq, Show)

-- fmap으로 더 편하게 정의할 수 있을까
instance Expr MinMax where
    con = MinMax
    add (MinMax x) (MinMax y) = MinMax (max x y)
    mul (MinMax x) (MinMax y) = MinMax (min x y)

instance Expr Mod7 where
    con n = Mod7 (n `mod` 7)
    add (Mod7 x) (Mod7 y) = Mod7 ( (x + y) `mod` 7)
    mul (Mod7 x) (Mod7 y) = Mod7 ( (x * y) `mod` 7)
 
testExp :: Expr a => Maybe a
testExp = parseExp con add mul "(3* -4) + 5"

testInteger = testExp :: Maybe Integer
testBool = testExp :: Maybe Bool
testMM = testExp :: Maybe MinMax
testSat = testExp :: Maybe Mod7
