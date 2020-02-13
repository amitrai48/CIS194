module Golf where

import Data.List

{-
skip function is an internal function which takes a list of type a and an index
whose multiples should be included in the return list. That is any other index 
that isn't the multiple of given index is excluded. 

In order to get index of the elems in the list I have used zip function. 
zip [1..] [9,10,11] = [(1,9), (2,10), (3,11)]. This returns a list
of pair with first being the index and second being the element. I have started 
from 1 based index because 0 `mod` anyNumber = 0 and the 1st elem will always be
included.

This list is passed to a filter which removes all the element whose index is
not divisble by the givrn index. Since finally we only need list of Integers 
we map over the list of pair and only take the second elem and finally return.
-}

skip :: [a] -> Int -> [a]
skip as i = map snd (filter (\(x,_) -> x `mod` i == 0) (zip [1..] as))

{- 
  Since we need to skip elements from 1 to length of the list skips maps over
  a range from 1 to length of list and applies the skip function.
-}
skips :: [a] -> [[a]]
skips as = map (skip as) [1..(length as)]

{-
localMaxima is defined as a recursive function which expects atleast 3 elems in
  a list and if the 2nd element is a local maxima cons this elem to the 
  returning list. Anything other than 3 elems returns an empty array. The func
  is called recusrively with 2nd elem and the rest of the list. 
-}

localMaxima :: [Integer] -> [Integer]
localMaxima [] = []
localMaxima (x:y:z:zs) 
  | x < y && y > z = y : localMaxima (y:z:zs)
  | otherwise = localMaxima (y:z:zs)
localMaxima _ = []

{- CountNum is a type alias where the first elem is the number and 
the 2nd is the frequency of the number present in a list
(2,4) would mean that 2 is present 4 times in a list
-}
type CountNum = (Integer, Int)

{- Histogram is a type alias for a list of String. 
This provides better documentation
-}
type Histogram = [String]

{-
concatStringByIndex takes 2 list of strings and concats them based on index.
That is element at same index are concated to form a new string. The length 
of the new list of String returned is equal to the max of the 2 length.

If they are of uneual length they are made of equal length by adding 
empty spaces. This is done by using replicate function. Once they are made of 
equal length both of the inputs are zipped together and finally concatted.

concatStringByIndex ["He", "Wo"] ["llo", "rld", "!"] = ["Hello", "World", "!"]
-}

concatStringByIndex :: [String] -> [String] -> [String]
concatStringByIndex x y
  | length x > length y = concatStringByIndex x  (y ++ replicate (length x - length y) "")
  | length x < length y = concatStringByIndex (x ++ replicate (length y - length x) "") y
  | otherwise = map (\(a,b) -> a ++ b) (zip x y)

{-
maxFreq takes a list of CountNum and returns the max frequency. Since the 
second element is frequency we use maximunBy and compare the second elem of the
pair. Finally the maximum CountNum's frequency i.e the second element is 
returned. There is a condition for empty list because maximumBy on empty list
throws excpetion.
-}
maxFreq :: [CountNum] -> Int
maxFreq [] = 0
maxFreq c = snd (maximumBy (\(_, x) (_, y) -> compare x y) c)

{-
frequency takes in a list of Integers and returns a list of CountNum. That is
Num with it's frequency. It starts by adding [0..9] to the given list. This is 
needed because if a number betweeen 0 to 9 is not present we still need it's
CountNum like (1,0) i.e 1 has 0 frequency. Since we are adding 0 to 9, when the 
actual frequency is calculated we substract 1 from the length.

Next we sort the list because we need to group them. After grouping all the 
identical elements are in one list. Something like this
for input [1,2,6,2,2,9] after grouping it becomes
[[0], [1,1], [2,2,2,2], [3], [4], [5], [6,6], [7], [8], [9,9]] Please note how 
an extra element is present in the list. Finally we map over this list of list 
and create CountNum by getting the head of the list i.e the number and the 
length of the list - 1. Subtracting 1 from length is important because we had 
added list 0 to 9 to our original list.

Caution this function would give an invalid output for list having number 
greater than 9 or less than 0. To solve that instead of adding a range 0 to 9 
to the list we can a range min to max of the given list
-}

frequency :: [Integer] -> [CountNum]
frequency xs = map (\x -> (head x, length x - 1)) (group (sort (xs ++ [0..9])))

{- 
createHistogram takes in CountNum and Max Frequency and returns an histogram 
for that CountNum. It takes in a max freq because it needs to add spaces to 
make all histograms of the same length. Having a equal sized histogram would 
make combinging them easy. 

It first creates a list of empty space paddings of length equal to the 
difference between maxFreq and it's own Freq. Than it creates a list of * of 
length equal to it's own freq and finally adds the ["=", "num"] to this list.

createHistogram (1,0) 5 = [" "," "," "," "," ","=","1"]
createHistogram (1,3) 5 = [" "," ","*","*","*","=","1"]
-}

createHistogram :: CountNum -> Int -> Histogram
createHistogram (num, freq) maxFreq = replicate (maxFreq - freq) " " ++ replicate freq "*" ++ ["=", show num]

{- 
histogram takes in a list of Integers and returns a String. It first converts 
list of Integer to list of CountNum by calling frequency. Then for each 
CountNum a Histogram is created. This is done by mapping over the list of 
CountNums over the function createHistogram. Max freq is also calculated and 
passed to createHistogram. This returns a list of Histograms. 

This list of Histograms are concatinated together and by folding and calling 
concatStringByIndex. This returns an [String]. Finally unlines converts 
[String] -> String by adding \n.
-}
histogram :: [Integer] -> String
histogram xs = unlines (foldl concatStringByIndex [] (map (`createHistogram` maxFreq (frequency xs)) (frequency xs)))