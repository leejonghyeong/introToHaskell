{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}
import Data.Text.Internal.Lazy.Fusion (stream)

-- First way to implement fibonacci
-- it takes exponential time.
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

-- Excercise 2

fibs2 :: [Integer]
fibs2 = scanl (+) 0 (1:fibs2)

-- fibs2 = [0,1,?,?,...]
-- fibs2 = scanl (+) 0 (1:[0,1,?,?,...])
--       = [0,1,1,2,?,?,...]
--       = scanl (+) 0 (1:[0,1,1,2,?,?,...])
--       = [0,1,1,2,3,5,?,?,...]
-- F(n) = F(n-1) + F(n-2)
-- fibs2 = scanl (+) 0 (1:[F0,F1,...])
--       = [0,1,F0+F1,F0+F1+F1,F0+F1+F1+F2,...]
--       = [0,1,F2,F3,F3+F2=F4,F4+F3=F5,F5+F4=F6,...]


--Excercise 3
--Define a type, Stream

data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons x xs) = x : streamToList xs

instance Show a => Show (Stream a) where
    show :: Stream a -> String
    show = show . take 20 . streamToList

-- Excercise 4
-- simple tools for Stream
streamRepeat :: a -> Stream a
streamRepeat x = Cons x (streamRepeat x)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x xs) = Cons (f x) (streamMap f xs)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = Cons x (streamMap f (streamFromSeed f x))

-- Exercise 5
-- Define Stream for natural number and their exponents of 2
nats :: Stream Integer
nats = streamFromSeed (+1) 1

-- Hint: define interleaveStreams
interleaveStreams :: Stream Integer -> Stream Integer -> Stream Integer
interleaveStreams (Cons x xs) (Cons y ys) = Cons x (Cons y (interleaveStreams xs ys))

ruler :: Stream Integer
ruler = Cons 0 (interleaveStreams (streamMap (+1) ruler) zero)
        where zero = streamRepeat 0

-- ruler = 0A0A0A... = inter 0 A
-- A's = 1B1B... = map (+1) 0A0A...
-- B's = 2C2C... = map (+1) 1B1B...
-- when we define something recurrently, we have to ensure
-- that the definition terminates

-- ruler = 0 A0A0A0... = 0 (inter A 0)
-- (A's = map (+1) 0 (inter A 0))

-- ruler = 0 (inter A 0)
--       = 0 (inter (map(+1) 0 ruler) 0)


-- Exercise 6
-- Fibonacci numbers via Generating functions

-- x = 0 + 1 x + 0 x^2 + ...
x :: Stream Integer
x = Cons 0 (Cons 1 (streamRepeat 0))

instance Num (Stream Integer) where
    fromInteger :: Integer -> Stream Integer
    fromInteger n = Cons n (streamRepeat 0)
    
    negate (Cons a0 a') = Cons (-a0) (negate a')
    
    (+) (Cons a0 a') (Cons b0 b') = Cons (a0+b0) ((+) a' b')
    
    (*) (Cons a0 a') b@(Cons b0 b') = Cons c ((+) a0b' a'b)
        where c = a0 * b0
              a0b' = streamMap (*a0) b'
              a'b = (*) a' b
    
instance Fractional (Stream Integer) where
    (/) a@(Cons a0 a') b@(Cons b0 b') = streamMap (`div` b0 ) (Cons a0 remainder)
        where remainder = (+) a' (negate qb')
              qb' = (*) q b'
              q = (/) a b

-- if f(x) = f0 + f1 x + f2 x^2 +..., then
-- x + x f(x) + x^2 f(x) = f(x) holds.
fibs3 :: Stream Integer
fibs3 = x / (1-x-x^2)


-- Exercise 7
-- compute nth fibonacci number with O(log n)

data Matrix a = Matrix a a a a
                deriving Show

instance Num a => Num (Matrix a) where
    (*) (Matrix a11 a12 a21 a22) (Matrix b11 b12 b21 b22) = Matrix c11 c12 c21 c22
        where c11 = a11 * b11 + a12 * b21
              c12 = a11 * b12 + a12 * b22
              c21 = a21 * b11 + a22 * b21
              c22 = a21 * b12 + a22 * b22
-- we could implement more operations like polynomials. but we don't need them

getFn :: Matrix Integer -> Integer
getFn (Matrix a11 a12 a21 a22) = a12

-- Note that the complexity of ^n is O(log n)
-- since ^n works as follows.
-- x^n = if even n then (x^(n `div` 2))^2 else x * (x^(n `div` 2))^2
-- So O(fib4) = O(log n)
fib4 :: Integer -> Integer
fib4 n = getFn (fibo^n)
    where fibo = Matrix 1 1 1 0