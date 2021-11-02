xor1 :: [Bool] -> Bool
xor1 = odd . length . filter (==True)

-- Implement xor using fold
xor :: [Bool] -> Bool
xor = foldl (\acc x -> if x then not acc else acc) False

-- xor2, point free expression
xor2 :: [Bool] -> Bool
xor2 = foldl ((==) . not) False

-- Implemet map using fold
map' :: (a->b) -> [a] -> [b]
map' f = foldr ((:).f) []

-- Implement foldl using foldr
myFoldl :: (a->b->a) -> a -> [b] -> a
myFoldl f base xs = foldr (flip f) base (reverse xs)

{- recall foldr and foldl work out as follows.
foldr f z [x1, x2, ..., xn] == x1 ‘f‘ (x2 ‘f‘ ... (xn ‘f‘ z)...)
foldl f z [x1, x2, ..., xn] == (...((z ‘f‘ x1) ‘f‘ x2) ‘f‘...) ‘f‘ xn

foldr (flip f) z [x1, x2, ..., xn] == (... (z ‘f‘ xn) ... ‘f‘ x2) ‘f‘ x1
foldr (flip f) z (reverse [x1, x2, ..., xn]) == (...((z ‘f‘ x1) ‘f‘ x2) ‘f‘...) ‘f‘ xn

ref: http://enshahar.com/haskell/cis194/high/order/programming/exercise/solution/2018/01/14/cis194-HighOrderProgramming-TypeInference-exanswer/

According to the above reference, we can implement foldl using foldr without reverse. Since foldr apply function (say g) from the right side, we need
x1 `g` (x2 `g` id) $ z = (z `f` x1) `f` x2 
i.e. (g x h) _ = h (f _ x) 
or g: x h _ -> h (f _ x)
-}

{-
Note that type of foldr is as follows
foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b
In our new myFoldl, b is a type of function c->c
-}
myFoldl2 :: (a->b->a) ->a -> [b] -> a
myFoldl2 f base xs = foldr (\x h y -> h (f y x) ) id xs base 
