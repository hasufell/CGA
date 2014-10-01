module Gtk where

import Control.Monad.IO.Class
import Diagram
import Diagrams.Prelude
import Diagrams.Backend.Cairo
import Diagrams.Backend.Cairo.Internal
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Windows.MessageDialog
import System.Directory


startGUI :: FilePath -> IO ()
startGUI startFile = do
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
  box1            <- vBoxNew False 0
  box2            <- hButtonBoxNew
  box3            <- hBoxNew False 0
  hscale          <- hScaleNewWithRange 0.0 10 0.5
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
  boxPackStart box3 fileButton PackGrow 5
  boxPackStart box3 hscale PackGrow 5

  -- adjust properties
  set window [windowDefaultWidth := 600, windowDefaultHeight := 700,
              windowTitle := "Computergrafik"]
  set box2 [buttonBoxLayoutStyle := ButtonboxCenter]
  containerSetBorderWidth box2 10
  _ <- windowSetTypeHint window WindowTypeHintDialog
  _ <- fileChooserSetCurrentFolder fileButton homedir
  _ <- fileChooserSetFilename fileButton startFile
  adjustment <- rangeGetAdjustment hscale
  _ <- adjustmentSetValue adjustment 2

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


onClickedSaveButton :: FileChooserButton
                    -> IO ()
onClickedSaveButton fcb = do
  filename <- fileChooserGetFilename fcb
  case filename of
    Just x -> do
      saveDiag' x
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


drawDiag' :: (WidgetClass widget, RangeClass scale)
          => FilePath
          -> widget
          -> scale
          -> IO ()
drawDiag' fp da scale' = do
  mesh       <- readFile fp
  dw         <- widgetGetDrawWindow da
  adjustment <- rangeGetAdjustment scale'
  scaleVal   <- adjustmentGetValue adjustment
  let (_, r) = renderDia Cairo
                 (CairoOptions "" (Width 600) SVG False)
                 (diagFromString (MkProp scaleVal) mesh)
  renderWithDrawable dw r


saveDiag' :: FilePath -> IO ()
saveDiag' fp = do
  mesh <- readFile fp
  renderCairo "out.svg" (Width 600) (diagFromString (MkProp 2) mesh)
