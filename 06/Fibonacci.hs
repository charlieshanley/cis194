{-# OPTIONS_GHC -Wall             #-}

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
streamToList (x :~ stream) = x : streamToList stream

----------
-- 4

streamRepeat :: a -> Stream a
streamRepeat x = x :~ streamRepeat x

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (x :~ xs) = f x :~ streamMap f xs

instance Functor Stream where
    fmap = streamMap

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = x :~ streamFromSeed f (f x)

----------
-- 5

nats :: Stream Integer
nats = streamFromSeed (+1) 0

ruler :: Stream Integer
ruler = go 0
    where go :: Integer -> Stream Integer
          go n = interleaveStreams (streamRepeat n) (go (n + 1))

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (x :~ xs) ys = x :~ interleaveStreams ys xs

-- cannot be the below because it pattern matches on both args, and when using
-- it in ruler, evaluating second arg never terminates
-- interleaveStreams :: Stream a -> Stream a -> Stream a
-- interleaveStreams (x :~ xs) (y :~ ys) = x :~ y :~ interleaveStreams xs ys


----------
-- 6

x :: Stream Integer

----------
-- 7

