module Golf where

{-
skip function is an internal function which takes a list of type a and an index
to skip (let's call this skip index) and returns an array with all the elements 
skipped whose index are multiple of the skip index. In order to get index of 
the elems in the list I have used zip function. 
zip [1..] [9,10,11] = [(1,9), (2,10), (3,11)]. This returns a list
of pair with first being the index and second being the element. I have started 
from 1 based index because 0 `mod` anyNumber = 0 and the 1st elem will always be
included.

This list is passed to a filter which removes all the element whose index is
divisble by the skip index. Since finally we only need list of a we take the
second of the pair and return.
-}

skip :: [a] -> Int -> [a]
skip as i = map snd (filter (\(x,_) -> x `mod` i == 0) (zip [1..] as))

{- 
  Since we need to skip elements from 1 to length of the list skips maps over
  a range from 1 to length of list and applies the skip function.
-}
skips :: [a] -> [[a]]
skips as = map (skip as) [1..(length as)]
