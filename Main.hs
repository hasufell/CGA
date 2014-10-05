import Gtk
import System.Environment


main :: IO ()
main = do
  a <- getArgs
  makeGUI (if null a then head a else "")
