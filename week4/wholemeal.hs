fun1' :: [Integer] -> Integer
fun1' [] = 1
fun1' (x:xs)
    | even x = (x-2) * fun1' xs
    | otherwise = fun1' xs

fun2' :: Integer -> Integer
fun2' 1 = 0
fun2' n | even n = n + fun2' (n `div` 2)
        | otherwise = fun2' (3 * n + 1)

--Reimplement these functions in more Haskell style.

fun1 :: [Integer] -> Integer
fun1 = foldl (\acc x -> (x-2) * acc) 1 . filter even

-- point free expression of (\acc x -> (x-2) * acc) is as follows.
-- (. subtract 2) . (*) 

fun2 :: Integer -> Integer 
fun2 = foldl (+) 0 . takeWhile (/= 1) . iterate (\x -> if even x then x `div` 2 else 3*x+1)
-- fold (+) 0 = sum
