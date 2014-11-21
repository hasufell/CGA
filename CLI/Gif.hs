{-# OPTIONS_HADDOCK ignore-exports #-}

module CLI.Gif where

import qualified Data.ByteString.Char8 as B
import Diagrams.Backend.Cairo.CmdLine
import Graphics.Diagram.Gif
import MyPrelude


gifCLI :: FilePath -> IO ()
gifCLI _ = do
  mesh <- B.readFile "UB1_sonderfaelle.obj"
  gifMain (gifDiagS def mesh)
