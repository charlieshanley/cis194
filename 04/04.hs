{-# OPTIONS_GHC -Wall #-}

module Idiomatic where

import Data.List (foldl')

putShow :: (Show a) => a -> IO ()
putShow = putStrLn . show

----------
-- 1

fun1, fun1' :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
    | even x    = (x - 2) * fun1 xs
    | otherwise = fun1 xs

fun1' = product . map (subtract 2) . filter even

test1 :: [Integer] -> IO ()
test1 n = (putShow $ fun1 n) >> (putShow $ fun1' n)


fun2, fun2' :: Integer -> Integer
fun2 1 = 0
fun2 n | even n    = n + fun2 (n `div` 2)
       | otherwise = fun2 (3 * n + 1)

fun2' = sum . filter even . takeWhile (>1) . iterate fun
    where fun x = if even x then div x 2 else 3 * x + 1

test2 :: Integer -> IO ()
test2 n = (putShow $ fun2 n) >> (putShow $ fun2' n)

----------
-- 2

data Tree a = Leaf | Node Integer (Tree a) a (Tree a) deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree = foldr insert Leaf

insert :: a -> Tree a -> Tree a
insert a Leaf = Node 1 Leaf a Leaf
insert a (Node _ l b r)
    | height l > height r = setHeight $ Node 0 l b $ insert a r
    | otherwise           = setHeight $ Node 0 (insert a l) b r


height :: Tree a -> Integer
height Leaf           = 0
height (Node h _ _ _) = h

setHeight :: Tree a -> Tree a
setHeight Leaf = Leaf
setHeight (Node _ l x r) = Node h' l' x r'
    where h' = 1 + max (height l') (height r')
          l' = setHeight l
          r' = setHeight r

testFoldTree :: Tree Char
testFoldTree = foldTree "The quick brown fox jumps over the lazy dog"

draw :: Show a => Tree a -> String
draw = draw' 0
    where
        draw' i tree = case tree of
            Leaf           -> indent ++ ".\n"
            (Node h l _ r) -> draw' j l ++ indent ++ show h ++"\n" ++ draw' j r
            where
                indent = replicate (i * 4) ' '
                j = i + 1

----------
-- 3

xor :: [Bool] -> Bool
xor = foldl' op False
    where op True  False = True
          op False True  = True
          op _     _     = False

map', map'' :: (a -> b) -> [a] -> [b]
map'  f = foldr (\x xs -> f x : xs) []
map'' f = foldr ((:) . f) []


myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f = foldr (flip f) 
-- ?

----------
-- 4

sieveSundaram :: Integer -> [Integer]
sieveSundaram = map ((+1) . (*2)) . sieve
    where
        sieve n = filter (not . (`elem` ij)) [1..n]
            where ij = [i + j + 2*i*j | i <- half, j <- half, i <= j]
                  half = [1..(div n 2)]

sieveSundaram' :: Integer -> [Integer]
sieveSundaram' n = map ((+1) . (*2)) . filter sieve $ [1..n]
    where
        sieve = not . (`elem` ij)
        ij = [i + j + 2*i*j | i <- half, j <- half, i <= j]
        half = [1..(div n 2)]
