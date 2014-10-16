{-# OPTIONS_HADDOCK ignore-exports #-}

module GUI.Gtk (makeGUI) where

import Control.Monad
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

  return MkMyGUI `ap`
    xmlGetWidget xml castToWindow "window1" `ap`
    xmlGetWidget xml castToButton "drawButton" `ap`
    xmlGetWidget xml castToButton "saveButton" `ap`
    xmlGetWidget xml castToButton "quitButton" `ap`
    xmlGetWidget xml castToFileChooserButton
            "filechooserButton" `ap`
    xmlGetWidget xml castToDrawingArea "drawingarea" `ap`
    xmlGetWidget xml castToHScale "hscale" `ap`
    xmlGetWidget xml castToEntry "xlD" `ap`
    xmlGetWidget xml castToEntry "xuD" `ap`
    xmlGetWidget xml castToEntry "ylD" `ap`
    xmlGetWidget xml castToEntry "yuD" `ap`
    xmlGetWidget xml castToAboutDialog "aboutdialog" `ap`
    xmlGetWidget xml castToComboBox "comboalgo" `ap`
    xmlGetWidget xml castToCheckButton "gridcheckbutton" `ap`
    xmlGetWidget xml castToCheckButton "coordcheckbutton"


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
  _ <- onClicked  (dB mygui) $ drawDiag mygui
  _ <- onClicked  (sB mygui) $ saveDiag mygui
  _ <- onClicked  (qB mygui) mainQuit
  _ <- onResponse (aD mygui) (\x -> case x of
                                ResponseCancel -> widgetHideAll (aD mygui)
                                _ -> return ())
  -- have to redraw for window overlapping and resizing on expose
  _ <- onExpose (da mygui) (\_ -> drawDiag mygui >>=
                             (\_ -> return True))
  _ <- on (cB mygui) changed (drawDiag mygui)
  _ <- on (gC mygui) toggled (drawDiag mygui)
  _ <- on (cC mygui) toggled (drawDiag mygui)

  -- hotkeys
  _ <- win mygui `on` keyPressEvent $ tryEvent $ do
    [Control] <- eventModifier
    "q"       <- eventKeyName
    liftIO mainQuit
  _ <- win mygui `on` keyPressEvent $ tryEvent $ do
    [Control] <- eventModifier
    "s"       <- eventKeyName
    liftIO $ saveDiag mygui
  _ <- win mygui `on` keyPressEvent $ tryEvent $ do
    [Control] <- eventModifier
    "d"       <- eventKeyName
    liftIO $ drawDiag mygui
  _ <- win mygui `on` keyPressEvent $ tryEvent $ do
    [Control] <- eventModifier
    "a"       <- eventKeyName
    liftIO $ widgetShowAll (aD mygui)

  -- draw widgets and start main loop
  widgetShowAll (win mygui)
  mainGUI


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
drawDiag :: MyGUI
          -> IO ()
drawDiag mygui = do
  fp <- fileChooserGetFilename (fB mygui)
  case fp of
    Just x -> do
      ret <- saveAndDrawDiag x "" mygui
      case ret of
        1 -> showErrorDialog "No valid x/y dimensions!"
        2 -> showErrorDialog "No valid Mesh file!"
        _ -> return ()
    Nothing -> return ()


-- |Saves a Diagram which is built from a given file as an SVG.
saveDiag :: MyGUI
          -> IO ()
saveDiag mygui = do
  fp <- fileChooserGetFilename (fB mygui)
  case fp of
    Just x -> do
      ret <- saveAndDrawDiag x "out.svg" mygui
      case ret of
        1 -> showErrorDialog "No valid x/y dimensions!"
        2 -> showErrorDialog "No valid Mesh file!"
        _ -> return ()
    Nothing -> return ()


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
