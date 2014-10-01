import Gtk
import System.Environment


main :: IO ()
main = do
  [a] <- getArgs
  startGUI a
