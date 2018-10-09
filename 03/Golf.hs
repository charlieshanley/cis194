
module Golf where

----------
-- 1

-- skips :: [a] -> [[a]]
-- skips []  = []
-- skips [x] = [[x]]
-- skips l@(x:xs) = l : 

----------
-- 2

localMaxima :: [Integer] -> [Integer]
localMaxima (x:y:z:l)
    | y > x && y > z = y : localMaxima (z:l)
    | otherwise      =     localMaxima (y:z:l)
localMaxima x = []

----------
-- 3

histogram :: [Integer] -> String
histogram xs = unlines rows
    where labels = concat $ show <$> [0..9]
          axis = "=========="
          vert_rows = 

histogram' :: [Integer] -> String
histogram' xs = fold [0..9] \n ->
    where labels = concat $ show <$> [0..9]
          axis = "=========="
