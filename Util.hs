module Util where


-- |Checks whether the Coordinates are in a given range.
inRange :: Double           -- ^ min
        -> Double           -- ^ max
        -> (Double, Double) -- ^ Coordinates to check
        -> Bool             -- ^ result
inRange min' max' (x, y)
  | x <= max' && x >= min' && y <= max' && y >= min' = True
  | otherwise = False


-- |Compare the extension of a file with the given String.
cmpExt :: String -> FilePath -> Bool
cmpExt _ [] = False
cmpExt checkExt fp
  | actualExt == fp = False
  | otherwise = checkExt == actualExt
    where
      actualExt = getExt fp


-- |Get the extension of a file.
getExt :: FilePath -> String
getExt = last           .
  splitBy (== '.')      .
  last                  .
  splitBy (== '/')


-- |Split an array into subarrays depending on a given condition.
splitBy :: (a -> Bool) -- ^ condition
        -> [a]         -- ^ array to split
        -> [[a]]       -- ^ splitted array
splitBy f s =  case dropWhile f s of
                 [] -> []
                 s' -> w : splitBy f s''
                   where (w, s'') = break f s'
