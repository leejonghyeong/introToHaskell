{-# LANGUAGE FlexibleContexts #-}

module Scrabble where
import Data.Char
import Data.Monoid

-- define score type and its instance of Monoid
newtype Score = Score Int
                deriving Show

instance Semigroup Score where
  (Score x) <> (Score y) = Score (x + y)

instance Monoid Score where
  mempty = Score 0

getScore :: Score -> Int
getScore (Score n) = n

score :: Char -> Score
score c
  | c1 `elem` ['A','E','I','O','N','R','T','L','S','U'] = Score 1
  | c1 `elem` ['D', 'G'] = Score 2
  | c1 `elem` ['B','C','M','P'] = Score 3
  | c1 `elem` ['F','H','V','W','Y'] = Score 4
  | c1 == 'K' = Score 5
  | c1 `elem` ['J','X'] = Score 8
  | c1 `elem` ['Q','Z'] = Score 10
  | otherwise = Score 0
    where c1 = toUpper c

scoreString :: String -> Score
scoreString = mconcat . map score 
-- (Default) mconcat = foldr mappend mempty
