{-# OPTIONS_HADDOCK ignore-exports #-}

module GUI.Gtk (makeGUI) where

import Control.Monad.IO.Class
import Diagrams.Prelude
import Diagrams.Backend.Cairo
import Diagrams.Backend.Cairo.Internal
import Graphics.Diagram.Gtk
import Graphics.Diagram.Types
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade
import MyPrelude
import System.Directory
import System.FileSystem.FileExt
import Text.Read


-- |Monolithic object passed to various GUI functions in order
-- to keep the API stable and not alter the parameters too much.
-- This only holds GUI widgets that are needed to be read during
-- runtime.
data MyGUI = MkMyGUI {
  -- |main Window
  win :: Window,
  -- |delete Button
  dB  :: Button,
  -- |save Button
  sB  :: Button,
  -- |quit Button
  qB  :: Button,
  -- |file chooser button
  fB  :: FileChooserButton,
  -- |drawing area
  da  :: DrawingArea,
  -- |scaler for point thickness
  hs  :: HScale,
  -- |entry widget for lower x bound
  xl  :: Entry,
  -- |entry widget for upper x bound
  xu  :: Entry,
  -- |entry widget for lower y bound
  yl  :: Entry,
  -- |entry widget for upper y bound
  yu  :: Entry,
  -- |about dialog
  aD  :: AboutDialog,
  -- |combo box for choosing the algorithm
  cB  :: ComboBox,
  -- |grid check button
  gC  :: CheckButton,
    -- |coord check button
  cC  :: CheckButton
}


-- |The glade file to load the UI from.
gladeFile :: FilePath
gladeFile = "GUI/gtk2.glade"


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
  gC'  <- xmlGetWidget xml castToCheckButton "gridcheckbutton"
  cC'  <- xmlGetWidget xml castToCheckButton "coordcheckbutton"

  return $ MkMyGUI win' dB' sB' qB' fB' da' hs'
                   xl' xu' yl' yu' aD' cB' gC' cC'


-- |Main entry point for the GTK GUI routines.
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
  -- have to redraw for window overlapping and resizing on expose
  _ <- onExpose (da mygui) (\_ -> onClickedDrawButton mygui >>=
                             (\_ -> return True))
  _ <- on (cB mygui) changed (onClickedDrawButton mygui)
  _ <- on (gC mygui) toggled (onClickedDrawButton mygui)
  _ <- on (cC mygui) toggled (onClickedDrawButton mygui)
  _ <- on (hs mygui) valueChanged (onClickedDrawButton mygui)

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
drawDiag' fp mygui = saveAndDrawDiag fp "" mygui


-- |Saves a Diagram which is built from a given file as an SVG.
saveDiag' :: FilePath
          -> MyGUI
          -> IO Int
saveDiag' fp mygui = saveAndDrawDiag fp "out.svg" mygui


-- |Draws and saves a Diagram which is built from a given file.
-- If the file to save is left empty, then nothing is saved.
saveAndDrawDiag :: FilePath -- ^ obj file to parse
                -> FilePath -- ^ if/where to save the result
                -> MyGUI
                -> IO Int
saveAndDrawDiag fp fps mygui =
  if cmpExt "obj" fp
    then do
      mesh       <- readFile fp
      dw         <- widgetGetDrawWindow (da mygui)
      adjustment <- rangeGetAdjustment (hs mygui)
      scaleVal   <- adjustmentGetValue adjustment
      xlD'       <- entryGetText (xl mygui)
      xuD'       <- entryGetText (xu mygui)
      ylD'       <- entryGetText (yl mygui)
      yuD'       <- entryGetText (yu mygui)
      alg'       <- comboBoxGetActive (cB mygui)
      (daW, daH) <- widgetGetSize (da mygui)
      gd'        <- toggleButtonGetActive (gC mygui)
      ct'        <- toggleButtonGetActive (cC mygui)

      let
        xD = (,)         <$>
          readMaybe xlD' <*>
          readMaybe xuD' :: Maybe (Double, Double)
        yD = (,)         <$>
          readMaybe ylD' <*>
          readMaybe yuD' :: Maybe (Double, Double)

      case (xD, yD) of
        (Just xD', Just yD') -> do
          let (s, r) = renderDia Cairo
                (CairoOptions fps
                   (Dims (fromIntegral daW) (fromIntegral daH))
                   SVG False)
                (diagS (def{
                    t   = scaleVal,
                    dX  = xD',
                    dY  = yD',
                    alg = alg',
                    gd  = gd',
                    ct  = ct'})
                  mesh)
          renderWithDrawable dw r
          if null fps
            then return ()
            else s
          return 0
        _ -> return 1

    else return 2
