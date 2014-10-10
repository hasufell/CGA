{-# OPTIONS_HADDOCK ignore-exports #-}

module System.FileSystem.FileExt where

import MyPrelude


-- |Compare the extension of a file with the given String.
cmpExt :: String -> FilePath -> Bool
cmpExt checkExt = (==) checkExt . getExt


-- |Get the extension of a file.
getExt :: FilePath -> String
getExt fp
  | hasExt fp = last .
    splitBy (== '.') .
    last             .
    splitBy (== '/') $
    fp
  | otherwise = ""


-- |Check if the file has an extension.
hasExt :: FilePath -> Bool
hasExt = (>1) . length . splitBy (== '.')
