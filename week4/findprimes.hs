import Data.List

cartProd :: [a] -> [b] -> [(a,b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]

-- let q = 2k+1 odd integer. q is not prime when k = i + j + 2ij
-- if then, q = (2i + 1) * (2j + 1)
-- Given n, find odd primes up to 2n + 2

sieveSundaram :: Integer -> [Integer]
sieveSundaram n = map (\x -> 2*x + 1) ([1..n] \\ [m | i <- [1..n], j <- [i..n], let m = i+j+2*i*j, m <= n])

{- 한계점
순다람 체는 log n 까지 시간복잡도를 줄일 수 있는데,
구현한 코드에서는 log n 이 아니라 n^2인것 같다.. (물론 i, j를 sqrt(n)으로 bound하면 n까지 줄일 수 있지만)
python에서는 while문을 사용해서 j를 i이상 그리고 i+j+2ij가 n 이하일때까지 고를 수 있지만 여기서는 그렇게 구현하기가 쉽지 않다.
-}