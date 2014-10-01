module Util where


-- |Checks whether the Coordinates are in a given range.
inRange :: Double           -- ^ min
        -> Double           -- ^ max
        -> (Double, Double) -- ^ Coordinates to check
        -> Bool             -- ^ result
inRange min' max' (x, y)
  | x <= max' && x >= min' && y <= max' && y >= min' = True
  | otherwise = False


cmpExt :: String -> FilePath -> Bool
cmpExt _ [] = False
cmpExt ext fp = ext == getExt fp


getExt :: FilePath -> String
getExt = last           .
  splitBy (== '.')      .
  last                  .
  splitBy (== '/')


splitBy :: (a -> Bool) -> [a] -> [[a]]
splitBy f s =  case dropWhile f s of
                 [] -> []
                 s' -> w : splitBy f s''
                   where (w, s'') = break f s'
