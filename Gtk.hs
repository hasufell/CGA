module Gtk where

import Control.Monad.IO.Class
import Control.Monad
import Defaults
import Diagram
import Diagrams.Prelude
import Diagrams.Backend.Cairo
import Diagrams.Backend.Cairo.Internal
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade
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
  xlDE       <- xmlGetWidget xml castToEntry "xlD"
  xlUE       <- xmlGetWidget xml castToEntry "xlU"
  ylDE       <- xmlGetWidget xml castToEntry "ylD"
  ylUE       <- xmlGetWidget xml castToEntry "ylU"

  -- adjust properties
  if startFile == ""
    then do
      _ <- fileChooserSetCurrentFolder fileButton homedir
      return ()
    else do
      _ <- fileChooserSetFilename fileButton startFile
      return ()

  -- callbacks
  _ <- onDestroy window mainQuit
  _ <- onClicked drawButton $ onClickedDrawButton fileButton
         da hscale (xlDE, xlUE) (ylDE, ylUE)
  _ <- onClicked saveButton $ onClickedSaveButton fileButton
         hscale (xlDE, xlUE) (ylDE, ylUE)
  _ <- onClicked quitButton mainQuit

  -- hotkeys
  _ <- window `on` keyPressEvent $ tryEvent $ do
         [Control] <- eventModifier
         "q"       <- eventKeyName
         liftIO mainQuit
  _ <- window `on` keyPressEvent $ tryEvent $ do
         [Control] <- eventModifier
         "s"       <- eventKeyName
         liftIO $ onClickedSaveButton fileButton
           hscale (xlDE, xlUE) (ylDE, ylUE)
  _ <- window `on` keyPressEvent $ tryEvent $ do
         [Control] <- eventModifier
         "d"       <- eventKeyName
         liftIO $ onClickedDrawButton fileButton da hscale
           (xlDE, xlUE) (ylDE, ylUE)

  -- draw widgets and start main loop
  widgetShowAll window
  mainGUI


-- |Callback when the "Draw" Button is clicked.
onClickedDrawButton :: (WidgetClass widget, RangeClass scale)
                    => FileChooserButton
                    -> widget
                    -> scale
                    -> (Entry, Entry)
                    -> (Entry, Entry)
                    -> IO ()
onClickedDrawButton fcb da scale' dXE dYE = do
  filename <- fileChooserGetFilename fcb
  case filename of
    Just x -> do
      cId <- onExpose da (\_ -> drawDiag' x da scale' dXE dYE)
      _   <- on fcb fileActivated (signalDisconnect cId)
      ret <- drawDiag' x da scale' dXE dYE
      unless ret $ showErrorDialog "No valid Mesh file!"
    Nothing -> showErrorDialog "No valid Mesh file!"


-- |Callback when the "Save" Button is clicked.
onClickedSaveButton :: RangeClass scale
                    => FileChooserButton
                    -> scale
                    -> (Entry, Entry)
                    -> (Entry, Entry)
                    -> IO ()
onClickedSaveButton fcb scale' dXE dYE = do
  filename <- fileChooserGetFilename fcb
  case filename of
    Just x -> do
      ret <- saveDiag' x scale' dXE dYE
      unless ret $ showErrorDialog "No valid Mesh file!"
    Nothing -> showErrorDialog "No valid Mesh file!"


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
drawDiag' :: (WidgetClass widget, RangeClass scale)
          => FilePath
          -> widget
          -> scale
          -> (Entry, Entry)
          -> (Entry, Entry)
          -> IO Bool
drawDiag' fp da scale' dXE dYE =
  if cmpExt "obj" fp
    then do
      mesh       <- readFile fp
      dw         <- widgetGetDrawWindow da
      adjustment <- rangeGetAdjustment scale'
      scaleVal   <- adjustmentGetValue adjustment
      xlD        <- entryGetText $ fst dXE
      xlU        <- entryGetText $ snd dXE
      ylD        <- entryGetText $ fst dYE
      ylU        <- entryGetText $ snd dYE

      -- clear drawing area
      clearDiag da

      let xD     = (read xlD, read xlU) :: (Double, Double)
          yD     = (read ylD, read ylU) :: (Double, Double)
          (_, r) = renderDia Cairo
                     (CairoOptions "" (Width 600) SVG False)
                     (diagFromString (def{t  = scaleVal,
                                          dX = xD,
                                          dY = yD})
                                     mesh)
      renderWithDrawable dw r
      return True
    else return False


-- |Saves a Diagram which is built from a given file as an SVG.
saveDiag' :: RangeClass scale
          => FilePath
          -> scale
          -> (Entry, Entry)
          -> (Entry, Entry)
          -> IO Bool
saveDiag' fp scale' dXE dYE =
  if cmpExt "obj" fp
    then do
      mesh       <- readFile fp
      adjustment <- rangeGetAdjustment scale'
      scaleVal   <- adjustmentGetValue adjustment
      xlD        <- entryGetText $ fst dXE
      xlU        <- entryGetText $ snd dXE
      ylD        <- entryGetText $ fst dYE
      ylU        <- entryGetText $ snd dYE

      let xD     = (read xlD, read xlU) :: (Double, Double)
          yD     = (read ylD, read ylU) :: (Double, Double)
      renderCairo "out.svg" (Width 600)
          (diagFromString (def{t  = scaleVal,
                               dX = xD,
                               dY = yD})
                           mesh)
      return True
    else return False


-- |Clears the drawing area by painting a white rectangle.
clearDiag :: WidgetClass widget
          => widget
          -> IO ()
clearDiag da = do
  dw <- widgetGetDrawWindow da

  let (_, r) = renderDia Cairo
               (CairoOptions "" (Width 600) SVG False)
               (emptyRect 600 600)
  renderWithDrawable dw r
