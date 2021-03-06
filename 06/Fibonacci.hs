{-# OPTIONS_GHC -Wall             #-}
{-# LANGUAGE FlexibleInstances    #-}

module Fibonacci where

----------
-- 1

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 2) + fib (n - 1)

fibs1 :: [Integer]
fibs1 = fmap fib [0..]

----------
-- 2

fibs2 :: [Integer]
fibs2 = 0 : 1 : zipWith (+) fibs2 (tail fibs2)

----------
-- 3

data Stream a = a :~ Stream a
infixr 5 :~

instance Show a => Show (Stream a) where
    show = (++ "...") . show . take 10 . streamToList

streamToList :: Stream a -> [a]
streamToList (a :~ stream) = a : streamToList stream

----------
-- 4

streamRepeat :: a -> Stream a
streamRepeat a = a :~ streamRepeat a

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (a :~ as) = f a :~ streamMap f as

instance Functor Stream where
    fmap = streamMap

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f a = a :~ streamFromSeed f (f a)

----------
-- 5

nats :: Stream Integer
nats = streamFromSeed (+1) 0

ruler :: Stream Integer
ruler = go 0
    where go :: Integer -> Stream Integer
          go n = interleaveStreams (streamRepeat n) (go (n + 1))

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (a :~ as) bs = a :~ interleaveStreams bs as

-- cannot be the below because it pattern matches on both args, and when using
-- it in ruler, evaluating second arg never terminates
-- interleaveStreams :: Stream a -> Stream a -> Stream a
-- interleaveStreams (x :~ xs) (y :~ ys) = x :~ y :~ interleaveStreams xs ys


----------
-- 6

x :: Stream Integer
x = 0 :~ 1 :~ streamRepeat 0

instance Num (Stream Integer) where
    fromInteger i    = i :~ streamRepeat 0
    negate (i :~ is) = negate i :~ negate is
    abs              = undefined
    signum           = undefined
    (a :~ as) +     (b :~ bs) = (a + b) :~ (as + bs)
    (a :~ as) * bs0@(b :~ bs) = (a * b) :~ (streamMap (* a) bs + (as * bs0))

instance Fractional (Stream Integer) where
    (a :~ as) / (b :~ bs) = q
        where q = (a `div` b) :~ streamMap (`div` b) (as - (q * bs))
    fromRational = undefined

fibs3 :: Stream Integer
fibs3 = x / (1 - x - x^(2 :: Integer))

-- Wow, dark mathemagics.

----------
-- 7

data Matrix = Matrix Integer Integer
                     Integer Integer -- wow, suprised this is parsed correctly

fmat :: Matrix
fmat = Matrix 1 1
              1 0 -- suprsised this is parsed correctly, too

instance Show Matrix where
    show (Matrix a11 a12 a21 a22) =
        show a11 ++ " " ++ show a12 ++ "\n" ++
        show a21 ++ " " ++ show a22

instance Num Matrix where
    (Matrix a11 a12 a21 a22) * (Matrix b11 b12 b21 b22) = Matrix 
        (a11*b11 + a12*b21) (a11*b12 + a12*b22)
        (a21*b11 + a22*b21) (a21*b12 + a22*b22)
    (+)         = undefined
    abs         = undefined
    signum      = undefined
    fromInteger = undefined
    negate      = undefined

fibs4 :: Integer -> Integer
fibs4 0 = 0
fibs4 n = fn
    where (Matrix _ fn _ _) = fmat^n


-- Wow, super fast! O(log(n))
