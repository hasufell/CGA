{-# OPTIONS_HADDOCK ignore-exports #-}

module Graphics.Diagram.Gtk where

import qualified Data.ByteString.Char8 as B
import Diagrams.Backend.Cairo
import Diagrams.Prelude
import Graphics.Diagram.Plotter
import Graphics.Diagram.Types
import Parser.Meshparser


-- |Create the Diagram from the points.
diag :: DiagProp -> Object -> Diagram Cairo R2
diag p obj@(Object _)
  | algo p == 0 =
      mkDiag (mconcat [coordPointsText, coordPoints, plotterBG])
             p obj
  | algo p == 1 =
      mkDiag (mconcat [convexHPText, convexHP, convexHLs, coordPoints, plotterBG])
             p obj
  | algo p == 4 =
      mkDiag (mconcat [quadPathSquare, squares, coordPointsText,
                       coordPoints, polyLines, plotterBG])
             p obj
  | otherwise = mempty
diag p objs@(Objects _)
  | algo p == 2 =
      mkDiag (mconcat [polyLines, coordPointsText, coordPoints, plotterBG])
             p objs
  | algo p == 3 =
      mkDiag (mconcat [polyIntersectionText, polyIntersection,
                      coordPoints, polyLines, plotterBG])
             p objs
  | otherwise = mempty


-- |Create the Diagram from a String which is supposed to be the contents
-- of an obj file.
diagS :: DiagProp -> B.ByteString -> Diagram Cairo R2
diagS p mesh
  | algo p == 2 || algo p == 3 =
      diag p
      . Objects
      . fmap (filterValidPT p)
      . facesToArr
      $ mesh
  | otherwise = (diag p . Object . filterValidPT p . meshToArr $ mesh) # bg white


-- |Create the tree diagram from a String which is supposed to be the contents
-- of an obj file.
diagTreeS :: DiagProp -> B.ByteString -> Diagram Cairo R2
diagTreeS p mesh
  | algo p == 4 = mkDiag treePretty p (Object
                                       . filterValidPT p
                                       . meshToArr
                                       $ mesh)
  | otherwise  = mempty
