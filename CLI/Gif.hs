{-# OPTIONS_HADDOCK ignore-exports #-}

module CLI.Gif where

import Diagrams.Backend.Cairo.CmdLine
import Graphics.Diagram.Gif
import MyPrelude


gifCLI :: FilePath -> IO ()
gifCLI _ = do
  mesh <- readFile "UB1_sonderfaelle.obj"
  gifMain (gifDiagS def mesh)
