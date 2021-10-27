-- The Towers of Hanoi
-- Example: hanoi 2 "a" "b" "c" == [("a","c"), ("a","c"), ("b","c")]

type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n a b c
    | n == 0 = []
    | otherwise = hanoi (n-1) a c b ++ [(a,c)] ++ hanoi (n-1) b a c

-- How about four peg hanoi?