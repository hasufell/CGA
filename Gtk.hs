module Gtk where

import Control.Monad.IO.Class
import Defaults
import Diagram
import Diagrams.Prelude
import Diagrams.Backend.Cairo
import Diagrams.Backend.Cairo.Internal
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade
import System.Directory
import Text.Read
import Util


data MyGUI = MkMyGUI {
  win :: Window,
  dB  :: Button,
  sB  :: Button,
  qB  :: Button,
  fB  :: FileChooserButton,
  da  :: DrawingArea,
  hs  :: HScale,
  xl  :: Entry,
  xu  :: Entry,
  yl  :: Entry,
  yu  :: Entry,
  aD  :: AboutDialog,
  cB  :: ComboBox
}


gladeFile :: FilePath
gladeFile = "gtk2.glade"


-- |Loads the glade file and creates the MyGUI object.
makeMyGladeGUI :: IO MyGUI
makeMyGladeGUI = do
  -- load glade file
  Just xml   <- xmlNew gladeFile
  win' <- xmlGetWidget xml castToWindow "window1"
  dB'  <- xmlGetWidget xml castToButton "drawButton"
  sB'  <- xmlGetWidget xml castToButton "saveButton"
  qB'  <- xmlGetWidget xml castToButton "quitButton"
  fB'  <- xmlGetWidget xml castToFileChooserButton
                  "filechooserButton"
  da'  <- xmlGetWidget xml castToDrawingArea "drawingarea"
  hs'  <- xmlGetWidget xml castToHScale "hscale"
  xl'  <- xmlGetWidget xml castToEntry "xlD"
  xu'  <- xmlGetWidget xml castToEntry "xuD"
  yl'  <- xmlGetWidget xml castToEntry "ylD"
  yu'  <- xmlGetWidget xml castToEntry "yuD"
  aD'  <- xmlGetWidget xml castToAboutDialog "aboutdialog"
  cB'  <- xmlGetWidget xml castToComboBox "comboalgo"

  return $ MkMyGUI win' dB' sB' qB' fB' da' hs' xl' xu' yl' yu' aD' cB'


-- |Handle the whole GTK gui.
makeGUI :: FilePath -> IO ()
makeGUI startFile = do
  homedir <- getHomeDirectory

  -- init gui
  _ <- initGUI

  -- get GUI object
  mygui <- makeMyGladeGUI

  -- adjust properties
  if startFile == ""
    then do
      _ <- fileChooserSetCurrentFolder (fB mygui) homedir
      return ()
    else do
      _ <- fileChooserSetFilename (fB mygui) startFile
      return ()
  comboBoxSetActive (cB mygui) 0

  -- callbacks
  _ <- onDestroy  (win mygui) mainQuit
  _ <- onClicked  (dB mygui) $ onClickedDrawButton mygui
  _ <- onClicked  (sB mygui) $ onClickedSaveButton mygui
  _ <- onClicked  (qB mygui) mainQuit
  _ <- onResponse (aD mygui) (\x -> case x of
                                ResponseCancel -> widgetHideAll (aD mygui)
                                _ -> return ())

  -- hotkeys
  _ <- win mygui `on` keyPressEvent $ tryEvent $ do
         [Control] <- eventModifier
         "q"       <- eventKeyName
         liftIO mainQuit
  _ <- win mygui `on` keyPressEvent $ tryEvent $ do
         [Control] <- eventModifier
         "s"       <- eventKeyName
         liftIO $ onClickedSaveButton mygui
  _ <- win mygui `on` keyPressEvent $ tryEvent $ do
         [Control] <- eventModifier
         "d"       <- eventKeyName
         liftIO $ onClickedDrawButton mygui
  _ <- win mygui `on` keyPressEvent $ tryEvent $ do
         [Control] <- eventModifier
         "a"       <- eventKeyName
         liftIO $ widgetShowAll (aD mygui)

  -- draw widgets and start main loop
  widgetShowAll (win mygui)
  mainGUI


-- |Callback when the "Draw" Button is clicked.
onClickedDrawButton :: MyGUI
                    -> IO ()
onClickedDrawButton mygui = do
  let fcb = fB mygui
  filename <- fileChooserGetFilename fcb
  case filename of
    Just x -> do
      cId <- onExpose (da mygui) (\_ -> drawDiag' x mygui >>=
                                        (\_ -> return True))
      _   <- on fcb fileActivated (signalDisconnect cId)
      ret <- drawDiag' x mygui
      case ret of
        1 -> showErrorDialog "No valid x/y dimensions!"
        2 -> showErrorDialog "No valid Mesh file!"
        _ -> return ()
    Nothing -> showErrorDialog "No valid Mesh file!"


-- |Callback when the "Save" Button is clicked.
onClickedSaveButton :: MyGUI
                    -> IO ()
onClickedSaveButton mygui = do
  filename <- fileChooserGetFilename (fB mygui)
  case filename of
    Just x -> do
      ret <- saveDiag' x mygui
      case ret of
        1 -> showErrorDialog "No valid x/y dimensions!"
        2 -> showErrorDialog "No valid Mesh file!"
        _ -> return ()
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
drawDiag' :: FilePath
          -> MyGUI
          -> IO Int
drawDiag' fp mygui =
  if cmpExt "obj" fp
    then do
      mesh       <- readFile fp
      dw         <- widgetGetDrawWindow (da mygui)
      adjustment <- rangeGetAdjustment (hs mygui)
      scaleVal   <- adjustmentGetValue adjustment
      xlD        <- entryGetText (xl mygui)
      xuD        <- entryGetText (xu mygui)
      ylD        <- entryGetText (yl mygui)
      yuD        <- entryGetText (yu mygui)

      -- clear drawing area
      clearDiag mygui

      let xD = (,) <$> readMaybe xlD <*> readMaybe xuD :: Maybe (Double,
                                                                 Double)
          yD = (,) <$> readMaybe ylD <*> readMaybe yuD :: Maybe (Double,
                                                                 Double)
      case (xD, yD) of
        (Just xD', Just yD') -> do
          let (_, r) = renderDia Cairo
                         (CairoOptions "" (Width 600) SVG False)
                         (diagFromString (def{t  = scaleVal,
                                              dX = xD',
                                              dY = yD'})
                                         mesh)
          renderWithDrawable dw r
          return 0
        _ -> return 1

    else return 2


-- |Saves a Diagram which is built from a given file as an SVG.
saveDiag' :: FilePath
          -> MyGUI
          -> IO Int
saveDiag' fp mygui =
  if cmpExt "obj" fp
    then do
      mesh       <- readFile fp
      adjustment <- rangeGetAdjustment (hs mygui)
      scaleVal   <- adjustmentGetValue adjustment
      xlD        <- entryGetText (xl mygui)
      xuD        <- entryGetText (xu mygui)
      ylD        <- entryGetText (yl mygui)
      yuD        <- entryGetText (yu mygui)

      let xD = (,) <$> readMaybe xlD <*> readMaybe xuD :: Maybe (Double,
                                                                 Double)
          yD = (,) <$> readMaybe ylD <*> readMaybe yuD :: Maybe (Double,
                                                                 Double)
      case (xD, yD) of
        (Just xD', Just yD') -> do
          renderCairo "out.svg" (Width 600)
            (diagFromString (def{t  = scaleVal,
                                 dX = xD',
                                 dY = yD'})
                             mesh)
          return 0
        _ -> return 1

    else return 2


-- |Clears the drawing area by painting a white rectangle.
clearDiag :: MyGUI
          -> IO ()
clearDiag mygui = do
  dw <- widgetGetDrawWindow (da mygui)

  let (_, r) = renderDia Cairo
               (CairoOptions "" (Width 600) SVG False)
               (emptyRect 600 600)
  renderWithDrawable dw r
