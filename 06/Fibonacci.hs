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

data Stream a = a `Cons` Stream a

instance Show a => Show (Stream a) where
    show = (++ "...") . show . take 10 . streamToList

streamToList :: Stream a -> [a]
streamToList (x `Cons` stream) = x : streamToList stream

----------
-- 4

streamRepeat :: a -> Stream a
streamRepeat x = x `Cons` streamRepeat x

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (x `Cons` xs) = f x `Cons` streamMap f xs

instance Functor Stream where
    fmap = streamMap

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = x `Cons` streamFromSeed f (f x)

----------
-- 5

nats :: Stream Integer
nats = streamFromSeed (+1) 0

ruler :: Stream Integer
ruler = undefined

----------
-- 6

----------
-- 7

