{-# LANGUAGE TupleSections #-}
{- CIS 194 HW 10
   due Monday, 1 April
-}

module AParser where

import           Control.Applicative

import           Data.Char
import Control.Applicative (Alternative)

-- A parser for a value of type a is a function which takes a String
-- represnting the input to be parsed, and succeeds or fails; if it
-- succeeds, it returns the parsed value along with the remainder of
-- the input.
newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

-- For example, 'satisfy' takes a predicate on Char, and constructs a
-- parser which succeeds only if it sees a Char that satisfies the
-- predicate (which it then returns).  If it encounters a Char that
-- does not satisfy the predicate (or an empty input), it fails.
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f [] = Nothing    -- fail on the empty input
    f (x:xs)          -- check if x satisfies the predicate
                        -- if so, return x along with the remainder
                        -- of the input (that is, xs)
        | p x       = Just (x, xs)
        | otherwise = Nothing  -- otherwise, fail

-- Using satisfy, we can define the parser 'char c' which expects to
-- see exactly the character c, and fails otherwise.
char :: Char -> Parser Char
char c = satisfy (== c)

{- For example:

*Parser> runParser (satisfy isUpper) "ABC"
Just ('A',"BC")
*Parser> runParser (satisfy isUpper) "abc"
Nothing
*Parser> runParser (char 'x') "xyz"
Just ('x',"yz")

-}

-- For convenience, we've also provided a parser for positive
-- integers.
posInt :: Parser Integer
posInt = Parser f
  where
    f xs
      | null ns   = Nothing
      | otherwise = Just (read ns, rest)
      where (ns, rest) = span isDigit xs

------------------------------------------------------------
-- Your code goes below here
------------------------------------------------------------

-- Exercise 1
-- Implement Functor instance for Parser
first :: (a -> b) -> (a,c) -> (b,c)
first f (x, c) = (f x, c)

instance Functor Parser where
  fmap f (Parser runparser) = Parser (fmap (first f) . runparser)

-- fmap :: a -> b -> fa -> fb
-- fa = Parser a = Parser (String -> Maybe (a, String))

-- Exercise 2
-- Implement Applicative instance for Parser

instance Applicative Parser where
  pure a = Parser (\s -> Just (a, s))
  (Parser p1) <*> (Parser p2) = Parser f
    where f s1 = case p1 s1 of
                Nothing -> Nothing
                Just (f, s2) -> case p2 s2 of
                                Nothing -> Nothing
                                Just (a2, s3) -> Just (f a2, s3)


-- pure :: a -> Parser a = Parser (String -> Maybe (a, String))
-- String -> Maybe (a, b, String)
-- <*> :: f (a->b) -> fa -> fb
--        Parser (String -> Maybe (a->b, String))


-- Exercise 3
-- Implement abParser using simple Parsers

-- runParser abParser "abcdef"
-- Just ((’a’,’b’),"cdef")
-- runParser abParser "aebcdf"
-- Nothing
abParser :: Parser (Char, Char)
abParser = (,) <$> char 'a' <*> char 'b'

abcParser :: Parser (Char, Char, Char)
abcParser = (,,) <$> char 'a' <*> char 'b' <*> char 'c'

-- runParser abParser_ "abcdef"
-- Just ((),"cdef")
-- runParser abParser_ "aebcdf"
-- Nothing
abParser_ :: Parser ()
abParser_ = (\s1 s2 -> ()) <$> char 'a' <*> char 'b'

--runParser intPair "12 34"
-- Just ([12,34],"")
intPair :: Parser [Integer]
intPair = (\n _ m -> [n,m]) <$> posInt <*> char ' ' <*> posInt


-- Exercise 4
-- Create Alternative Instance for Parser

instance Alternative Parser where
  empty = Parser (const Nothing)
  (Parser p1) <|> (Parser p2) = Parser f
    where f s = case p1 s of
                Just (a1, s1) -> Just (a1, s1)
                Nothing -> p2 s

-- Exercise 5
-- Implement Parser which parses either Integer or Uppercase Char and fails otherwise
intOrUppercase :: Parser ()
intOrUppercase = const () <$> posInt <|> const () <$> satisfy isUpper

-- Note: const () <$> Parser == (() <$ Parser)
