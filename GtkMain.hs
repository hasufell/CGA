{-# OPTIONS_HADDOCK ignore-exports #-}

import GUI.Gtk
import System.Environment


main :: IO ()
main = do
  a <- getArgs
  makeGUI (if null a then "" else head a)
