{-# OPTIONS_GHC -Wall #-}

--------
-- ex 1

toDigitsRev :: Integer -> [Integer]
toDigitsRev n = rev (toDigits n)

toDigits :: Integer -> [Integer]
toDigits n 
  | n > 0     = map read (map (:[]) (show n))
  | otherwise = []

rev :: [a] -> [a]
rev []     = []
rev (x:xs) = rev xs ++ [x]

--------
-- ex 2

-- I decided to start from the front, because I already have toDigitsRev
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther (x:y:xs) = x : (2 * y) : doubleEveryOther xs
doubleEveryOther xs       = xs

--------
-- ex 3

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs) = sum (toDigits x) + sumDigits xs

--------
-- ex 4

validate :: Integer -> Bool
validate n = checksum n `mod` 10 == 0

checksum :: Integer -> Integer
checksum = sumDigits . doubleEveryOther . rev . toDigits

--------
-- ex 5

type Peg  = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n _ _ _ | n < 1 = []
hanoi 1 a b _         = [(a, b)]
hanoi n a b c         = hanoi (n - 1) a c b ++ [(a, b)] ++ hanoi (n - 1) c b a

correct :: Bool
correct = hanoi 2 "a" "b" "c" == [("a", "c"), ("a", "b"), ("c", "b")]
