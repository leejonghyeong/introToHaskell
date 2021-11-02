import Control.Arrow (ArrowChoice(right))
data Tree a = Leaf 
            | Node Integer (Tree a) a (Tree a)
            deriving (Show, Eq)

height :: Tree a -> Integer
height Leaf = -1
height (Node h _ _ _) = h

isBalanced :: Tree a -> Bool
isBalanced Leaf = False
isBalanced (Node h left _ right)
    | h <= 1 = isBalanced left == isBalanced right
    | otherwise = (height left == height right) && isBalanced left && isBalanced right

insert :: a -> Tree a -> Tree a
insert n Leaf = Node 0 Leaf n Leaf
insert n tree@(Node h left x right)
    | isBalanced tree = Node (h+1) (insert n left) x right
    | not (isBalanced left) = Node h (insert n left) x right
    | otherwise = Node h left x (insert n right)


foldTree :: [a] -> Tree a
foldTree = foldr insert Leaf
