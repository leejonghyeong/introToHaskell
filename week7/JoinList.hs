{-# LANGUAGE FlexibleInstances #-}
module JoinList where
import Sized
import Scrabble
import Buffer
import Editor

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
                    deriving (Eq, Show)

-- get the annotation at the root of JoinList
tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m1 _) = m1
tag (Append m1 _ _) = m1

-- append two JoinList
(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) x y = Append (m1 `mappend` m2) x y
            where m1 = tag x
                  m2 = tag y

-- indexing into JoinList
indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty = Nothing
indexJ i (Single _ x)
  | i < 0 = Nothing
  | otherwise = Just x
indexJ i (Append m1 left right)
  | i >= l1 = Nothing
  | i >= l2 = indexJ (i- l2) right
  | otherwise = indexJ i left
    where l1 = getSize (size m1)
          l2 = getSize (size (tag left)) 

-- drop the first n elements from JoinList
dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ _ Empty = Empty
dropJ n jl@(Single {})
  | n > 0 = Empty
  | otherwise = jl
dropJ n (Append m1 left right)
  | n > l1 = Empty
  | n > l2 = dropJ (n - l2) right
  | otherwise = (+++) (dropJ n left) right
    where l1 = getSize (size m1)
          l2 = getSize (size (tag left))

-- take the first n elements from JoinList
takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ _ Empty = Empty
takeJ n jl@(Single {})
  | n > 0 = jl
  | otherwise = Empty
takeJ n jl@(Append m1 left right)
  | n > l1 = jl
  | n > l2 = (+++) left (takeJ (n-l2) right)
  | otherwise = takeJ n left
    where l1 = getSize (size m1)
          l2 = getSize (size (tag left))


-- test out JoinLists annotated with scores
scoreLine :: String -> JoinList Score String
scoreLine [] = Empty
scoreLine s = Single score s
  where score = scoreString s

sizeLine :: String -> JoinList Size String
sizeLine [] = Empty
sizeLine s = Single (Size 1) s

bufferLine :: String -> JoinList (Score, Size) String
bufferLine s = Single (score, Size 1) s
  where score = scoreString s

-- make a function to run editor interface using JoinList backend
-- in place of slow String backend
instance Buffer (JoinList (Score, Size) String) where
  toString jl = case jl of
                Empty -> ""
                Single _ s -> s
                Append _ left right -> toString left ++ toString right

  -- need map from string to JoinList (Score, Size) String
  -- instead of scoreLine
  fromString = foldr ((+++) . bufferLine) Empty . lines
  
  line = indexJ

  replaceLine n l b = takeJ (n-1) b +++ bufferLine l +++ dropJ n b

  numLines = getSize . snd . tag

  value = getScore . fst . tag

-- 공백 포함하도록 수정하기


main = runEditor editor buffer
  where buffer =  foldr ((+++) . bufferLine) Empty
         [ "This buffer is for notes you don't want to save, and for"
         , "evaluation of steam valve coefficients."
         , "To load a different file, type the character L followed"
         , "by the name of the file."
         ]
