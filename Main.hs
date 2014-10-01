import Diagram
import Diagrams.Prelude
import Diagrams.Backend.Cairo
import Diagrams.Backend.Cairo.Internal
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Windows.MessageDialog
import System.Directory


main :: IO ()
main = do
  homedir <- getHomeDirectory

  -- init gui
  _ <- initGUI

  -- create window and widgets
  window          <- windowNew
  da              <- drawingAreaNew
  fileButton      <- fileChooserButtonNew "Select mesh"
                     FileChooserActionOpen
  drawButton      <- buttonNew
  saveButton      <- buttonNew
  quitButton      <- buttonNew
  box1 <- vBoxNew False 0
  box2 <- hButtonBoxNew
  box3 <- hButtonBoxNew
  drawButtonLabel <- labelNew $ Just "Draw"
  saveButtonLabel <- labelNew $ Just "Save"
  quitButtonLabel <- labelNew $ Just "Quit"

  -- containers and boxing
  containerAdd drawButton drawButtonLabel
  containerAdd saveButton saveButtonLabel
  containerAdd quitButton quitButtonLabel
  containerAdd window box1
  boxPackStart box1 da PackGrow 0
  boxPackStart box1 box2 PackNatural 0
  boxPackStart box1 box3 PackNatural 0
  boxPackStart box2 drawButton PackNatural 0
  boxPackStart box2 saveButton PackNatural 0
  boxPackStart box2 quitButton PackNatural 0
  boxPackStart box3 fileButton PackNatural 0

  -- adjust properties
  set window [windowDefaultWidth := 600, windowDefaultHeight := 700,
              windowTitle := "Computergrafik"]
  set box2 [buttonBoxLayoutStyle := ButtonboxCenter]
  set box3 [buttonBoxLayoutStyle := ButtonboxCenter]
  _ <- windowSetTypeHint window WindowTypeHintDialog
  containerSetBorderWidth box2 10
  _ <- fileChooserSetCurrentFolder fileButton homedir

  -- callbacks
  _ <- onDestroy window mainQuit
  _ <- onClicked drawButton $ onClickedDrawButton fileButton
                                                  da
  _ <- onClicked saveButton $ onClickedSaveButton fileButton
  _ <- onClicked quitButton mainQuit

  -- draw widgets and start main loop
  widgetShowAll window
  mainGUI


onClickedDrawButton :: WidgetClass widget
                    => FileChooserButton
                    -> widget
                    -> IO ()
onClickedDrawButton fcb da = do
  filename <- fileChooserGetFilename fcb
  case filename of
    Just x -> do
      mesh <- readFile x
      dw   <- widgetGetDrawWindow da
      let (_, r) = renderDia Cairo
                     (CairoOptions "" (Width 600) SVG False)
                     (diagFromString mesh)
      renderWithDrawable dw r
    Nothing -> do
      showErrorDialog "No valid Mesh file!"


onClickedSaveButton :: FileChooserButton
                    -> IO ()
onClickedSaveButton fcb = do
  filename <- fileChooserGetFilename fcb
  case filename of
    Just x -> do
      mesh <- readFile x
      let (png, _) = renderDia Cairo
                       (CairoOptions "out.svg" (Width 600) SVG False)
                       (diagFromString mesh)
      png
    Nothing -> do
      showErrorDialog "No valid Mesh file!"


showErrorDialog :: String -> IO ()
showErrorDialog str = do
  errorDialog <- messageDialogNew Nothing
                                  [DialogDestroyWithParent]
                                  MessageError
                                  ButtonsClose
                                  str
  _ <- dialogRun errorDialog
  widgetDestroy errorDialog
