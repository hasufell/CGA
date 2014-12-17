{-# OPTIONS_HADDOCK ignore-exports #-}

module GUI.Gtk (makeGUI) where

import Algebra.Vector (dimToSquare)
import Control.Applicative
import Control.Monad(unless)
import Control.Monad.IO.Class
import qualified Data.ByteString.Char8 as B
import Data.Maybe
import Diagrams.Prelude
import Diagrams.Backend.Cairo
import Diagrams.Backend.Cairo.Internal
import Graphics.Diagram.Core (DiagProp(..))
import Graphics.Diagram.Gtk
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade
import MyPrelude
import System.Directory
import System.FilePath.Posix
import Text.Read


-- |Monolithic object passed to various GUI functions in order
-- to keep the API stable and not alter the parameters too much.
-- This only holds GUI widgets that are needed to be read during
-- runtime.
data MyGUI = MkMyGUI {
  -- |main Window
  rootWin :: Window,
  -- |Tree Window
  treeWin :: Window,
  -- |delete Button
  delButton :: Button,
  -- |save Button
  saveButton :: Button,
  -- |quit Button
  quitButton :: Button,
  -- |file chooser button
  fileButton :: FileChooserButton,
  -- |drawing area
  mainDraw :: DrawingArea,
  -- |drawing area for the tree
  treeDraw :: DrawingArea,
  -- |scaler for point thickness
  ptScale :: HScale,
  -- |entry widget for lower x bound
  xminEntry :: Entry,
  -- |entry widget for upper x bound
  xmaxEntry :: Entry,
  -- |entry widget for lower y bound
  yminEntry :: Entry,
  -- |entry widget for upper y bound
  ymaxEntry :: Entry,
  -- |about dialog
  aboutDialog :: AboutDialog,
  -- |combo box for choosing the algorithm
  algoBox :: ComboBox,
  -- |grid check button
  gridCheckBox :: CheckButton,
  -- |coord check button
  coordCheckBox :: CheckButton,
  -- |Path entry widget for the quad tree.
  quadPathEntry :: Entry,
  -- |Horizontal box containing the path entry widget.
  vbox7 :: Box,
  -- |Horizontal box containing the Rang search entry widgets.
  vbox10 :: Box,
  -- |Range entry widget for lower x bound
  rangeXminEntry :: Entry,
  -- |Range entry widget for upper x bound
  rangeXmaxEntry :: Entry,
  -- |Range entry widget for lower y bound
  rangeYminEntry :: Entry,
  -- |Range entry widget for upper y bound
  rangeYmaxEntry :: Entry
}


-- |The glade file to load the UI from.
gladeFile :: FilePath
gladeFile = "GUI/gtk2.glade"


-- |Loads the glade file and creates the MyGUI object.
makeMyGladeGUI :: IO MyGUI
makeMyGladeGUI = do
  -- load glade file
  Just xml   <- xmlNew gladeFile

  MkMyGUI
    <$> xmlGetWidget xml castToWindow "window1"
    <*> xmlGetWidget xml castToWindow "window2"
    <*> xmlGetWidget xml castToButton "drawButton"
    <*> xmlGetWidget xml castToButton "saveButton"
    <*> xmlGetWidget xml castToButton "quitButton"
    <*> xmlGetWidget xml castToFileChooserButton "filechooserButton"
    <*> xmlGetWidget xml castToDrawingArea "drawingarea"
    <*> xmlGetWidget xml castToDrawingArea "treedrawingarea"
    <*> xmlGetWidget xml castToHScale "hscale"
    <*> xmlGetWidget xml castToEntry "xlD"
    <*> xmlGetWidget xml castToEntry "xuD"
    <*> xmlGetWidget xml castToEntry "ylD"
    <*> xmlGetWidget xml castToEntry "yuD"
    <*> xmlGetWidget xml castToAboutDialog "aboutdialog"
    <*> xmlGetWidget xml castToComboBox "comboalgo"
    <*> xmlGetWidget xml castToCheckButton "gridcheckbutton"
    <*> xmlGetWidget xml castToCheckButton "coordcheckbutton"
    <*> xmlGetWidget xml castToEntry "path"
    <*> xmlGetWidget xml castToBox "vbox7"
    <*> xmlGetWidget xml castToBox "vbox10"
    <*> xmlGetWidget xml castToEntry "rxMin"
    <*> xmlGetWidget xml castToEntry "rxMax"
    <*> xmlGetWidget xml castToEntry "ryMin"
    <*> xmlGetWidget xml castToEntry "ryMax"


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
      _ <- fileChooserSetCurrentFolder (fileButton mygui) homedir
      return ()
    else do
      _ <- fileChooserSetFilename (fileButton mygui) startFile
      return ()
  comboBoxSetActive (algoBox mygui) 0

  -- callbacks
  _ <- onDestroy  (rootWin mygui)     mainQuit
  _ <- onClicked  (delButton mygui)   $ drawDiag mygui
  _ <- onClicked  (saveButton mygui)  $ saveDiag mygui
  _ <- onClicked  (quitButton mygui)  mainQuit
  _ <- onResponse (aboutDialog mygui)
         (\x -> case x of
            ResponseCancel -> widgetHideAll (aboutDialog mygui)
            _              -> return ())
  -- have to redraw for window overlapping and resizing on expose
  _ <- onExpose (mainDraw mygui) (\_ -> drawDiag mygui >>=
                                        (\_ -> return True))
  _ <- onExpose (treeDraw mygui) (\_ -> drawDiag mygui >>=
                                        (\_ -> return True))
  _ <- on (algoBox mygui)       changed (drawDiag mygui)
  _ <- on (algoBox mygui)       changed (onAlgoBoxChange mygui)
  _ <- on (gridCheckBox mygui)  toggled (drawDiag mygui)
  _ <- on (coordCheckBox mygui) toggled (drawDiag mygui)

  -- hotkeys
  _ <- rootWin mygui `on` keyPressEvent $ tryEvent $ do
    [Control] <- eventModifier
    "q"       <- eventKeyName
    liftIO mainQuit
  _ <- treeWin mygui `on` keyPressEvent $ tryEvent $ do
    [Control] <- eventModifier
    "q"       <- eventKeyName
    liftIO (widgetHide $ treeWin mygui)
  _ <- rootWin mygui `on` keyPressEvent $ tryEvent $ do
    [Control] <- eventModifier
    "s"       <- eventKeyName
    liftIO $ saveDiag mygui
  _ <- rootWin mygui `on` keyPressEvent $ tryEvent $ do
    [Control] <- eventModifier
    "d"       <- eventKeyName
    liftIO $ drawDiag mygui
  _ <- rootWin mygui `on` keyPressEvent $ tryEvent $ do
    [Control] <- eventModifier
    "a"       <- eventKeyName
    liftIO $ widgetShowAll (aboutDialog mygui)

  -- draw widgets and start main loop
  widgetShowAll (rootWin mygui)
  widgetShowAll (treeWin mygui)
  widgetHide (vbox7 mygui)
  widgetHide (vbox10 mygui)
  widgetHide (treeWin mygui)
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


-- |May hide or show the widget that holds the quad tree path entry,
-- depending on the context and may also pop up the tree window.
onAlgoBoxChange :: MyGUI
                -> IO ()
onAlgoBoxChange mygui = do
  item <- comboBoxGetActive (algoBox mygui)
  if item == 4
    then do
      widgetHide (vbox10 mygui)
      widgetShow (vbox7 mygui)
      widgetShow (treeWin mygui)
    else
      if item == 5
        then do
          widgetHide (vbox7 mygui)
          widgetShow (vbox10 mygui)
          widgetShow (treeWin mygui)
      else do
        widgetHide (vbox10 mygui)
        widgetHide (vbox7 mygui)
        widgetHide (treeWin mygui)

  return ()


-- |Draws a Diagram which is built from a given file to
-- the gtk DrawingArea.
drawDiag :: MyGUI
          -> IO ()
drawDiag mygui = do
  fp <- fileChooserGetFilename (fileButton mygui)
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
  fp <- fileChooserGetFilename (fileButton mygui)
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
  if (==) ".obj" . takeExtension $ fp
    then do
      mesh            <- B.readFile fp
      mainDrawWindow  <- widgetGetDrawWindow (mainDraw mygui)
      treeDrawWindow  <- widgetGetDrawWindow (treeDraw mygui)
      adjustment      <- rangeGetAdjustment (ptScale mygui)
      scaleVal        <- adjustmentGetValue adjustment
      xminEntryText   <- entryGetText (xminEntry mygui)
      xmaxEntryText   <- entryGetText (xmaxEntry mygui)
      yminEntryText   <- entryGetText (yminEntry mygui)
      ymaxEntryText   <- entryGetText (ymaxEntry mygui)
      algoActive      <- comboBoxGetActive (algoBox mygui)
      (daW, daH)      <- widgetGetSize (mainDraw mygui)
      (daTW, daTH)    <- widgetGetSize (treeDraw mygui)
      gridActive      <- toggleButtonGetActive (gridCheckBox mygui)
      coordTextActive <- toggleButtonGetActive (coordCheckBox mygui)
      quadPathEntry'  <- entryGetText (quadPathEntry mygui)
      rxminEntryText   <- entryGetText (rangeXminEntry mygui)
      rxmaxEntryText   <- entryGetText (rangeXmaxEntry mygui)
      ryminEntryText   <- entryGetText (rangeYminEntry mygui)
      rymaxEntryText   <- entryGetText (rangeYmaxEntry mygui)

      let
        xDim = (,)         <$>
          readMaybe xminEntryText <*>
          readMaybe xmaxEntryText :: Maybe (Double, Double)
        yDim = (,)         <$>
          readMaybe yminEntryText <*>
          readMaybe ymaxEntryText :: Maybe (Double, Double)
        rxDim = (,)         <$>
          readMaybe rxminEntryText <*>
          readMaybe rxmaxEntryText :: Maybe (Double, Double)
        ryDim = (,)         <$>
          readMaybe ryminEntryText <*>
          readMaybe rymaxEntryText :: Maybe (Double, Double)
        renderDiag winWidth winHeight buildDiag =
          renderDia Cairo
            (CairoOptions fps
               (Dims (fromIntegral winWidth) (fromIntegral winHeight))
               SVG False)
            (buildDiag (def{
                dotSize       = scaleVal,
                xDimension    = fromMaybe (0, 500) xDim,
                yDimension    = fromMaybe (0, 500) yDim,
                algo          = algoActive,
                haveGrid      = gridActive,
                showCoordText = coordTextActive,
                quadPath      = quadPathEntry',
                rangeSquare   = case (rxDim, ryDim) of
                  (Just xd', Just yd') -> dimToSquare xd' yd'
                  _                    -> ((0, 0), (500, 500))
                })
              mesh)
        (s, r)  = renderDiag daW daH diagS
        (_, r') = renderDiag daTW daTH diagTreeS

      renderWithDrawable mainDrawWindow r
      renderWithDrawable treeDrawWindow r'

      unless (null fps) s
      return 0

    else return 2
