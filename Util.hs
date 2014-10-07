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
