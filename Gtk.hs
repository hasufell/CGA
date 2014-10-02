module Gtk where

import Control.Monad.IO.Class
import Diagram
import Diagrams.Prelude
import Diagrams.Backend.Cairo
import Diagrams.Backend.Cairo.Internal
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade
import Graphics.UI.Gtk.Windows.MessageDialog
import System.Directory
import Util


gladeFile :: FilePath
gladeFile = "gtk2.glade"

-- |Handle the whole GTK gui.
makeGUI :: FilePath -> IO ()
makeGUI startFile = do
  homedir <- getHomeDirectory

  -- init gui
  _ <- initGUI

  -- load glade file
  Just xml   <- xmlNew gladeFile
  window     <- xmlGetWidget xml castToWindow "window1"
  drawButton <- xmlGetWidget xml castToButton "drawButton"
  saveButton <- xmlGetWidget xml castToButton "saveButton"
  quitButton <- xmlGetWidget xml castToButton "quitButton"
  fileButton <- xmlGetWidget xml castToFileChooserButton
                  "filechooserButton"
  da         <- xmlGetWidget xml castToDrawingArea "drawingarea"
  hscale     <- xmlGetWidget xml castToHScale "hscale"

  -- adjust properties
  _ <- fileChooserSetCurrentFolder fileButton homedir
  _ <- fileChooserSetFilename fileButton startFile

  -- callbacks
  _ <- onDestroy window mainQuit
  _ <- onClicked drawButton $ onClickedDrawButton fileButton
                                                  da hscale
  _ <- onClicked saveButton $ onClickedSaveButton fileButton
  _ <- onClicked quitButton mainQuit

  -- hotkeys
  _ <- window `on` keyPressEvent $ tryEvent $ do
         [Control] <- eventModifier
         "q"       <- eventKeyName
         liftIO $ mainQuit
  _ <- window `on` keyPressEvent $ tryEvent $ do
         [Control] <- eventModifier
         "s"       <- eventKeyName
         liftIO $ onClickedSaveButton fileButton
  _ <- window `on` keyPressEvent $ tryEvent $ do
         [Control] <- eventModifier
         "d"       <- eventKeyName
         liftIO $ onClickedDrawButton fileButton da hscale

  -- draw widgets and start main loop
  widgetShowAll window
  mainGUI


-- |Callback when the "Draw" Button is clicked.
onClickedDrawButton :: (WidgetClass widget, RangeClass scale)
                    => FileChooserButton
                    -> widget
                    -> scale
                    -> IO ()
onClickedDrawButton fcb da scale' = do
  filename <- fileChooserGetFilename fcb
  case filename of
    Just x -> do
      drawDiag' x da scale'
    Nothing -> do
      showErrorDialog "No valid Mesh file!"


-- |Callback when the "Save" Button is clicked.
onClickedSaveButton :: FileChooserButton
                    -> IO ()
onClickedSaveButton fcb = do
  filename <- fileChooserGetFilename fcb
  case filename of
    Just x -> do
      saveDiag' x
    Nothing -> do
      showErrorDialog "No valid Mesh file!"


-- |Pops up an error Dialog with the given String.
showErrorDialog :: String -> IO ()
showErrorDialog str = do
  errorDialog <- messageDialogNew Nothing
                                  [DialogDestroyWithParent]
                                  MessageError
                                  ButtonsClose
                                  str
  _ <- dialogRun errorDialog
  widgetDestroy errorDialog


-- |Draws a Diagram which is built from a given file to
-- the gtk DrawingArea.
-- Prints an error dialog if no valid mesh file is found.
drawDiag' :: (WidgetClass widget, RangeClass scale)
          => FilePath
          -> widget
          -> scale
          -> IO ()
drawDiag' fp da scale' =
  case cmpExt "obj" fp of
    True -> do
      mesh       <- readFile fp
      dw         <- widgetGetDrawWindow da
      adjustment <- rangeGetAdjustment scale'
      scaleVal   <- adjustmentGetValue adjustment
      let (_, r) = renderDia Cairo
                     (CairoOptions "" (Width 600) SVG False)
                     (diagFromString (MkProp scaleVal) mesh)
      renderWithDrawable dw r
    False -> showErrorDialog "No valid Mesh file!"


-- |Saves a Diagram which is built from a given file as an SVG.
-- Prints an error dialog if no valid mesh file is found.
saveDiag' :: FilePath -> IO ()
saveDiag' fp =
  case cmpExt "obj" fp of
    True -> do
      mesh <- readFile fp
      renderCairo "out.svg" (Width 600) (diagFromString (MkProp 2) mesh)
    False -> showErrorDialog "No valid Mesh file!"
