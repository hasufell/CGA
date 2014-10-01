module Util where


-- |Checks whether the Coordinates are in a given range.
inRange :: Double           -- ^ min
        -> Double           -- ^ max
        -> (Double, Double) -- ^ Coordinates to check
        -> Bool             -- ^ result
inRange min' max' (x, y)
  | x <= max' && x >= min' && y <= max' && y >= min' = True
  | otherwise = False


