import Gtk
import System.Environment


main :: IO ()
main = do
  a <- getArgs
  case null a of
    False -> startGUI (head a)
    True  -> startGUI ""
