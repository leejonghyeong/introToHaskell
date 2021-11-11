module Party where

import Employee
import Data.Tree

-- We are going to invite employees so that their sum of fun is max
-- but there is problem that employee has 0 fun when her immediate boss was also invited.
-- Employee hierarchy has (rose) Tree structure
-- Find the best GuestList

-- Exercise 1
-- Just add new employee and her fun score
glCons :: Employee -> GuestList -> GuestList 
glCons emp@(Emp _ fun) (GL emplist total) = GL (emp: emplist) (total + fun)

-- Define Monoid instance for GuestList
instance Semigroup GuestList where
  (GL list1 fun1) <> (GL list2 fun2) = foldr f (GL list2 fun2) list1
    where f emp gl@(GL list fun)
            | emp `elem` list = gl
            | otherwise = glCons emp gl 

instance Monoid GuestList where
  mempty = GL [] 0

-- Return more fun GuestList
moreFun :: GuestList -> GuestList -> GuestList 
moreFun gl1 gl2
  | gl1 > gl2 = gl1
  | otherwise = gl2

-- Exercise 2
-- Implement foldr on rose tree structure
treeFold :: (a -> b -> b) -> b -> Tree a -> b
treeFold f z (Node rootLabel subForest) = foldr (flip (treeFold f)) z' subForest
  where z' = f rootLabel z


-- Exercise 3
-- Given boss of some tree and two best GuestList for each subtrees,
-- Find two GuestLists of entire tree (first one invite the boss while second doesn't)
nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel boss [] = (GL [boss] (empFun boss), GL [] 0)
nextLevel boss list = mconcat . map (\(fst, snd) -> (glCons boss snd, moreFun fst snd)) $ list

-- Exercise 4
-- Find a fun maximizing GuestList
whichFun :: (GuestList, GuestList) -> GuestList
whichFun (gl1, gl2) = moreFun gl1 gl2

maxFunTuple :: Tree Employee -> (GuestList, GuestList)
maxFunTuple (Node boss subforest) = nextLevel boss sublist
  where sublist = map maxFunTuple subforest

maxFun :: Tree Employee -> GuestList
maxFun = whichFun . maxFunTuple

-- Exercise 5
getFun :: GuestList -> Fun
getFun (GL _ fun) = fun

getList :: GuestList -> [Name]
getList (GL list _) = map (\(Emp name _) -> name) list

main :: IO()
main = do
  contents <- readFile "company.txt"
  let gl = maxFun $ read contents
      fun = getFun gl
      list = getList gl
  putStr "Total fun: " >> print fun
  mapM_ putStrLn list

