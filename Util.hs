{-# OPTIONS_HADDOCK ignore-exports #-}

module Util where


-- |Split an array into subarrays depending on a given condition.
splitBy :: (a -> Bool) -- ^ condition
        -> [a]         -- ^ array to split
        -> [[a]]       -- ^ splitted array
splitBy f s =  case dropWhile f s of
                 [] -> []
                 s' -> w : splitBy f s''
                   where (w, s'') = break f s'


-- |Remove a given item from a list.
removeItem :: (Eq a) => a -> [a] -> [a]
removeItem x = foldr (\x' y -> if x' == x then y else x':y) []
