{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module LogAnalysis where
import Log

parseMessage :: String -> LogMessage
parseMessage s = case words s of
                [] -> Unknown s
                (_ : [])-> Unknown s
                (x:y:zs)
                    | x == "I" -> LogMessage Info (read y :: Int) (unwords zs)
                    | x == "I" -> LogMessage Warning (read y :: Int) (unwords zs)
                    | x == "E" -> LogMessage (Error (read y :: Int)) (read (head zs) :: Int) (unwords (tail zs))
                    | otherwise -> Unknown s

parse :: String -> [LogMessage]
parse s = map parseMessage (lines s)

{-
getTimestamp :: LogMessage -> Int
getTimestamp (Unknown _) = 0
getTimestamp (LogMessage _ n _) = n

insertInList :: LogMessage -> [LogMessage] -> [LogMessage]
insertInList message (x:xs) = case message of
    (Unknown _) -> x:xs
    (LogMessage {})
        | n >= m -> x : insertInList message xs
        | otherwise -> message : x : xs
            where n = getTimestamp message
                  m = getTimestamp x

toList :: MessageTree -> [LogMessage]
toList Leaf = []
toList (Node tree1 message tree2) = toList tree1 ++ [message] ++ toList tree2

toTree :: [LogMessage] -> MessageTree
toTree [] = Leaf
toTree xs = Node
            (toTree $ take half xs)
            (xs !! half)
            (toTree $ drop (half+1) xs)
                where half = length xs `div` 2


insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert message Leaf = Node Leaf message Leaf
insert message tree = toTree $ insertInList message (toList tree)
-}

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert msg Leaf = Node Leaf msg Leaf
insert msg1@(LogMessage _ n1 _) (Node left msg2@(LogMessage _ n2 _) right)
    | n1 >= n2 = Node left msg2 (insert msg1 right)
    | n1 < n2 = Node (insert msg1 left) msg2 right

build :: [LogMessage] -> MessageTree
build = foldl (flip insert) Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left msg right) = inOrder left ++ [msg] ++ inOrder right

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong [] = []
whatWentWrong ((LogMessage (Error n) _ msg) : xs)
    | n >= 50 = msg : whatWentWrong xs
whatWentWrong ((LogMessage {}) : xs) = whatWentWrong xs
whatWentWrong ((Unknown _) : xs) = whatWentWrong xs
                        