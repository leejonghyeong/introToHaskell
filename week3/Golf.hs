module Golf where
import Data.List (elemIndex, transpose, group, sort)


skips :: [a] -> [[a]]
skips s = [ [ s !! j | j <- [0..len -1], (j+1) `mod` (i+1) == 0] | i <- [0..len - 1]]
    where len = length s

localMaxima :: [Integer] -> [Integer]
localMaxima [] = []
localMaxima [_] = []
localMaxima [_,_] = []
localMaxima (x:y:z:zs)
    | y > x && y > z = y : localMaxima (z:zs)
    | otherwise = localMaxima (y:z:zs)

histogram :: [Integer] -> String
histogram xs = drawStar $ reverse. transpose . group . sort $ xs

drawStar :: [[Integer]] -> String
drawStar [] = replicate 10 '=' ++ "\n" ++ ['0'..'9'] ++ "\n"
drawStar (xs:xss) = [ if i `elem` xs then '*' else ' '|i <- [0..9]]  ++ "\n" ++ drawStar xss