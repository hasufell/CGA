{-# OPTIONS_HADDOCK ignore-exports #-}

module MyPrelude where

import Control.Arrow ((***))
import Data.List


-- |Used to create a common interface for default settings of data types.
class Def a where
  def :: a


-- |For negating random types.
class Not b where
  not' :: b -> b


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


-- |Get the last x elements of a list.
getLastX :: Int -> [a] -> [a]
getLastX a xs
  | length xs < a = []
  | otherwise     = snd . splitAt (length xs - a) $ xs


-- |Get a list with it's head and last element cut. If there are less
-- than 2 elements in the list, return an empty list.
tailInit :: [a] -> [a]
tailInit xs
  | length xs > 2 = tail . init $ xs
  | otherwise     = []


-- |Remove duplicates from a list.
rmdups :: (Ord a) => [a] -> [a]
rmdups =
  foldl (\seen x -> if x `elem` seen
    then seen
    else seen ++ [x]) []


-- |Apply a function to the first element of a tuple.
first :: (a -> b) -> (a,c) -> (b,c)
first = (*** id)


-- |Sequentialize a list, such as:
-- [1, 2, 3, 4, 5] -> [[1],[1,2],[1,2,3],[1,2,3,4],[1,2,3,4,5]]
seqList :: [a] -> [[a]]
seqList = tail. inits


-- |Duplicate the last element of a list and append it.
dupLast :: [a] -> [a]
dupLast [] = []
dupLast xs = xs ++ [last xs]


-- |Simpler version of if-then-else.
if' :: Bool -> a -> a -> a
if' True  x _ = x
if' False _ y = y


-- |Shift a list k places.
shiftM :: Int -> [a] -> [a]
shiftM _ [] = []
shiftM 0 xs = xs
shiftM n xs = drop n xs ++ take n xs


-- |Get the pivot of a list.
pivot :: [a] -> Maybe a
pivot [] = Nothing
pivot xs = Just . (!!) xs . flip div 2 . length $ xs
