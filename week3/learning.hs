-- Recursion patterns
-- map - perform an operation on every element of a list

data IntList = Empty
  | Cons Int IntList
  deriving Show

absAll :: IntList -> IntList
absAll Empty = Empty
absAll (Cons x xs) = Cons (abs x) (absAll xs)

squareAll :: IntList -> IntList
squareAll Empty = Empty
squareAll (Cons x xs) = Cons (x * x) (squareAll xs)

mapIntList :: (Int -> Int) -> IntList -> IntList
mapIntList _ Empty = Empty
mapIntList f (Cons x xs) = Cons (f x) (mapIntList f xs)

square :: Int -> Int
square x = x * x

addOne :: Int -> Int
addOne x = x + 1

exampleList :: IntList
exampleList = Cons (-1) (Cons (-5) (Cons 2 (Cons 5 Empty)))

-- mapIntList addOne exampleList Cons 0 (Cons (-4) (Cons 3 (Cons 6 Empty)))
-- mapIntList abs    exampleList Cons 1 (Cons 5 (Cons 2 (Cons 5 Empty)))
-- mapIntList square exampleList Cons 1 (Cons 25 (Cons 4 (Cons 25 Empty)))

-- Filter

keepOnlyEven :: IntList -> IntList
keepOnlyEven Empty = Empty
keepOnlyEven (Cons x xs)
  | even x = Cons x (keepOnlyEven xs)
  | otherwise = keepOnlyEven xs


filterIntList :: (Int -> Bool) -> IntList -> IntList
filterIntList _ Empty = Empty
filterIntList f (Cons x xs)
  | f x = Cons x (filterIntList f xs)
  | otherwise = filterIntList f xs

-- Polymorphism
-- Haskell supports polymorphism for both data types and functions.

-- polymorphic data type
data List t = E | C t (List t)
  deriving Show

-- t is a type variable, which can stand for any type
-- type variables must start with a lowercase letter, whereas types must start with uppercase
-- data List t = ... means List type is parameterized by a type

lst1 :: List Int
lst1 = C 1 (C 2 (C 3 E))

lst2 :: List Char
lst2 = C 'H' (C 'E' (C 'L' (C 'L' (C 'O' E))))

lst3 :: List Bool
lst3 = C True (C False E)

-- polymorphic functions
filterList :: (t -> Bool) -> List t -> List t
filterList _ E = E
filterList f (C x xs)
  | f x = C x (filterList f xs)
  | otherwise = filterList f xs

-- see how the f is of type a -> b and not just a -> a.
mapList :: (a -> b) -> List a -> List b
mapList _ E = E
mapList f (C x xs) = C (f x) (mapList f xs)

-- The Prelude, module with standard definitions that get implicitly imported into every Haskell program
