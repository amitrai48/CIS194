-- Enumeration types
data Thing = Shoe
  | Ship
  | SealingWax
  | Cabbage
  | King
  deriving Show 

-- Shoe, SealingWax are data constructors

isSmall :: Thing -> Bool
isSmall Shoe = True
isSmall Ship = False
isSmall SealingWax = True
isSmall Cabbage = True
isSmall King = False

isSmall2 :: Thing -> Bool
isSmall2 Ship = False
isSmall2 King = False
isSmall2 _ = True

-- Beyond enumeration

data FailableDouble = Failure
  | OK Double
  deriving Show

-- Failure is a data constructor with no args so Failure by itself is of type FailableDouble
-- OK is a data constructor with one args, OK by itself is not of type FailableDouble; it needs a Double

-- :t Failure = Failure :: FailableDouble
-- :t OK = OK :: Double -> FailableDouble

safeDiv :: Double -> Double -> FailableDouble
safeDiv _ 0 = Failure
safeDiv a b = OK (a / b)

-- destructuring using pattern matching
failureToZero :: FailableDouble -> Double
failureToZero Failure = 0
failureToZero (OK d) = d

-- Data constructors with more than one args
data Person = Person String Int Thing
  deriving Show

-- Person as type constructor is different than Person as data constructor

brent :: Person
brent = Person "Brent" 31 SealingWax

stan :: Person
stan = Person "Stan" 94 Cabbage

getAge :: Person -> Int
getAge (Person _ a _) = a


-- ADT has one or more data constructors and each data constructor can have zero or more arguments
-- Type constructor and data constructors must always start with a capital letter
-- variables including names of function must always start with a lower letter

-- Pattern matching
-- Pattern matching is about taking apart a value by finding out which constructor it was built with

-- defining function is a syntatic sugar for defining a case expression.
failureToZero' :: FailableDouble -> Double
failureToZero' x = case x of
  Failure -> 0
  OK d -> d

data FailableInt = FailedInt 
  | OKInt Int
  deriving Show

toInt :: String -> FailableInt
toInt s = case reads s :: [(Int, String)] of
  [(a, "")] -> OKInt a
  _ -> FailedInt