--validating credit card

reverseInt :: [Integer] -> [Integer]
reverseInt [] = []
reverseInt (x:xs) = reverseInt xs ++ [x]

lengthInt :: Integer -> Int
lengthInt xs = length (show xs)



toDigitsRev :: Integer -> [Integer]
toDigitsRev n
  | n <= 0 = []
  | otherwise = (n `mod` 10) : toDigitsRev (n `div` 10)

toDigits :: Integer -> [Integer]
toDigits n = reverseInt (toDigitsRev n)

doubleEveryOtherFromFirst :: [Integer] -> [Integer]
doubleEveryOtherFromFirst [] = []
doubleEveryOtherFromFirst [x] = [x]
doubleEveryOtherFromFirst (x:y:zs) = [x, y * 2] ++ doubleEveryOtherFromFirst zs

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs = reverseInt( doubleEveryOtherFromFirst (reverseInt xs) )

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs)
  | lengthInt x == 1 = x + sumDigits xs
  | otherwise = sumDigits( toDigitsRev x) + sumDigits xs
  
validate :: Integer -> Bool
validate x = sumDigits ( doubleEveryOther (toDigits x) ) `mod` 10 == 0  

type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 a b c = []
hanoi n a b c = hanoi (n - 1) a c b ++ ((a, b) : hanoi (n - 1) c b a)