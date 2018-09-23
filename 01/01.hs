
toDigits :: Integer -> [Integer]
toDigits n 
  | n > 0     = map read (map (:[]) (show n))
  | otherwise = []
  
--------

toDigitsRev :: Integer -> [Integer]
toDigitsRev n = rev (toDigits n)

rev :: [a] -> [a]
rev []     = []
rev (x:xs) = rev xs ++ [x]

--------

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther (x:y:xs) = x : (2 * y) : doubleEveryOther xs
doubleEveryOther xs       = xs

--------

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs) = sum (toDigits x) + sumDigits xs

--------

validate :: Integer -> Bool
validate n = checksum n `mod` 10 == 0

checksum = sumDigits . doubleEveryOther . rev . toDigits

--------

type Peg  = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n _ _ _ | n < 1 = []
hanoi 1 a b _ = [(a, b)]
hanoi n a b c = hanoi (n - 1) a c b ++ [(a, b)] ++ hanoi (n - 1) c b a

correct = hanoi 2 "a" "b" "c" == [("a", "c"), ("a", "b"), ("c", "b")]
