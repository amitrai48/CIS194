-- what benefits does having functions as first class have?
-- evaluating expressions rather than executing instructions? Benefits of this?

x :: Int
x = 3

y :: Int
y = y + 1

biggestInt, smallestInt :: Int
biggestInt = maxBound
smallestInt = minBound

n :: Integer
n = 123456789012345678912345678990

reallyBig :: Integer
reallyBig = 2^(2^(2^(2^2)))

numDigits :: Int
numDigits = length (show reallyBig)

sumtorial :: Integer -> Integer
sumtorial 0 = 0
sumtorial n = n + sumtorial(n-1)

hailstone :: Integer -> Integer
hailstone n
  | n `mod` 2 == 0 = n `div` 2
  | otherwise = 3*n + 1

foo :: Integer -> Integer
foo 0 = 16
foo 1
  | "Haskell" > "C++" = 3
  | otherwise = 4
foo n
  | n < 0 = 0
  | n `mod` 17 == 2 = -43
  | otherwise = n + 3

-- isEven :: Integer -> Bool
-- isEven n
--   | n `mod` 2 == 0 = True
--   | otherwise = False

isEven :: Integer -> Bool
isEven n = n `mod` 2 == 0

sumPair :: (Int, Int) -> Int
sumPair (x, y) = x + y

hailstoneSeq :: Integer -> [Integer]
hailstoneSeq 1 = [1]
hailstoneSeq n = n : hailstoneSeq (hailstone n) 

intListLength :: [Integer] -> Integer
intListLength [] = 0
intListLength (_:xs) = 1 + intListLength xs

hailstoneLen :: Integer -> Integer
hailstoneLen n = intListLength (hailstoneSeq n) - 1