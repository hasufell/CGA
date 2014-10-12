{-# OPTIONS_HADDOCK ignore-exports #-}

module MyPrelude where

import Data.List


-- |Used to create a common interface for default settings of data types.
class Def a where
  def :: a


-- |Split an array into subarrays depending on a given condition.
splitBy :: (a -> Bool) -- ^ condition
        -> [a]         -- ^ array to split
        -> [[a]]       -- ^ splitted array
splitBy f s =
  case dropWhile f s of
    [] -> []
    s' -> w : splitBy f s''
      where (w, s'') = break f s'


-- |Remove a given item from a list.
removeItem :: (Eq a) => a -> [a] -> [a]
removeItem x = foldr (\x' y -> if x' == x then y else x':y) []


-- |Sort a liste of tuples lexicographically.
sortLex :: (Ord a) => [(a, a)] -> [(a, a)]
sortLex =
  sortBy (\(x1, y1) (x2, y2) -> case compare x1 x2 of
           EQ -> compare y1 y2
           x  -> x)


-- |Get a list with it's head and last element cut. If there are less
-- than 2 elements in the list, return an empty list.
tailInit :: [a] -> [a]
tailInit xs
  | length xs > 2 = tail . init $ xs
  | otherwise     = []


-- |Apply a function to the first element of a tuple.
first :: (a -> b) -> (a,c) -> (b,c)
first f (x,y) = (f x, y)


-- |Sequentialize a list, such as:
-- [1, 2, 3, 4, 5] -> [[1],[1,2],[1,2,3],[1,2,3,4],[1,2,3,4,5]]
seqList :: [a] -> [[a]]
seqList = reverse . takeWhile (not . null) . iterate init


-- |Duplicate the last element of a list and append it.
dupLast :: [a] -> [a]
dupLast [] = []
dupLast xs = xs ++ [last xs]
