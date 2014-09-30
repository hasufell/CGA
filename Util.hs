module Util where


-- |Checks whether the Coordinates are in a given range.
inRange :: (Double, Double) -- ^ Coordinates to check
        -> Double           -- ^ min
        -> Double           -- ^ max
        -> Bool             -- ^ result
inRange (x, y) min' max'
  | x <= max' && x >= min' && y <= max' && y >= min' = True
  | otherwise = False


-- |Filter the valid coordinates.
filterValidCoords :: Double             -- ^ min
                    -> Double             -- ^ max
                    -> [(Double, Double)] -- ^ unfiltered
                    -> [(Double, Double)] -- ^ filtered
filterValidCoords min' max' = filter (\(x, y) -> inRange (x, y) min' max')
