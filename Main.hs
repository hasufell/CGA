import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Diagram
import Diagrams.Prelude
import Diagrams.Backend.Cairo
import Diagrams.Backend.Cairo.Internal
import Graphics.UI.Gtk


main :: IO ()
main = do
  _ <- initGUI
  mesh <- readFile "test.obj"
  window <- windowNew
  da <- drawingAreaNew
  set window [windowDefaultWidth := 700, windowDefaultHeight := 700,
              windowTitle := "Computergrafik", containerBorderWidth := 10,
              containerChild := da]
  _ <- onDestroy window mainQuit
  void $ da `on` exposeEvent $ liftIO $ do
      dw <- widgetGetDrawWindow da
      let (png, r) = renderDia Cairo
                             (CairoOptions "jo.svg" (Width 600) SVG False)
                             (diagFromString mesh)
      png
      renderWithDrawable dw r
      return True
  _ <- windowSetTypeHint window WindowTypeHintDialog
  widgetShowAll window
  mainGUI
