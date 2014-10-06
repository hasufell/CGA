{-# OPTIONS_HADDOCK ignore-exports #-}

module Util where


-- |Checks whether the Coordinates are in a given dimension.
inRange :: (Double, Double) -- ^ X dimension
        -> (Double, Double) -- ^ Y dimension
        -> (Double, Double) -- ^ Coordinates
        -> Bool             -- ^ result
inRange (xlD, xuD) (ylD, yuD) (x,y)
  | x <= xuD && x >= xlD &&
    y <= yuD && y >= ylD    = True
  | otherwise               = False


-- |Compare the extension of a file with the given String.
cmpExt :: String -> FilePath -> Bool
cmpExt checkExt = (==) checkExt . getExt


-- |Get the extension of a file.
getExt :: FilePath -> String
getExt fp
  | hasExt fp = last   .
      splitBy (== '.') .
      last             .
      splitBy (== '/') $
      fp
  | otherwise = ""


-- |Check if the file has an extension.
hasExt :: FilePath -> Bool
hasExt = (>1) . length . splitBy (== '.')


-- |Split an array into subarrays depending on a given condition.
splitBy :: (a -> Bool) -- ^ condition
        -> [a]         -- ^ array to split
        -> [[a]]       -- ^ splitted array
splitBy f s =  case dropWhile f s of
                 [] -> []
                 s' -> w : splitBy f s''
                   where (w, s'') = break f s'
