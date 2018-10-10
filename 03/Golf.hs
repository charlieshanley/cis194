{-# OPTIONS_GHC -Wall #-}

module Golf where

import Data.List (transpose)

----------
-- 1

skips :: [a] -> [[a]]
skips xs = flip skip xs `map` [0 .. (length xs - 1)]
    where
        skip :: Int -> [a] -> [a]
        skip n ys = case drop n ys of
            z:zs -> z : skip n zs
            []   -> []


----------
-- 2

localMaxima :: [Int] -> [Int]
localMaxima (x:y:z:l)
    | y > x && y > z = y : localMaxima (z:l)
    | otherwise      =     localMaxima (y:z:l)
localMaxima _ = []


----------
-- 3

histogram :: [Int] -> String
histogram xs = unlines $ rows ++ ["==========\n0123456789"]
    where
        rows = reverse . transpose . fill . map stars $ [0..9]

        stars :: Int -> String
        stars n = replicate (length $ filter (==n) xs) '*'

        fill :: [String] -> [String]
        fill ys = pad len ' ' `map` ys
            where len = maximum $ map length ys

        pad :: Int -> a -> [a] -> [a]
        pad n a = take n . (++ repeat a)


-- testing purposes

putHist :: [Int] -> IO ()
putHist = putStr . histogram

test :: IO ()
test = putHist [1,1,8,4,5,6,3,4,3,4,3,2,1,8,4,5,4,5,7,6,7,0,9,0]


-------------------------------------------------------------------------------
-- Allowing all tricks to make them short

----------
-- 1

skips' :: [a] -> [[a]]
skips' xs = skip xs <$> [0 .. (length xs - 1)]
    where
        skip ys n = case drop n ys of { (z:zs) -> z : skip zs n; [] -> [] }

----------
-- 2

----------
-- 3

histogram' :: [Int] -> String
histogram' xs = unlines $ rows ++ ["==========\n0123456789"]
    where
        rows = reverse . transpose . fill $ stars <$> [0..9]
        stars n = replicate (length $ filter (==n) xs) '*'
        fill l = take (maximum $ length <$> l) . (++ repeat ' ') <$> l

