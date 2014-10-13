{-# OPTIONS_HADDOCK ignore-exports #-}

module Graphics.Diagram.Gtk where

import Algebra.VectorTypes
import Diagrams.Backend.Cairo
import Diagrams.Prelude
import Graphics.Diagram.Plotter
import Graphics.Diagram.Types
import Parser.Meshparser


-- |Create the Diagram from the points.
diag :: DiagProp -> [PT] -> Diagram Cairo R2
diag p = case alg p of
  0 ->
    mkDiag
      (mconcat [maybeDiag (ct p) coordPointsText,
        coordPoints, xAxis, yAxis,
        maybeDiag (gd p) grid, whiteRectB])
      p
  1 ->
    mkDiag
      (mconcat
        [maybeDiag (ct p) convexHPText,
        convexHP, convexHLs,
        coordPoints, xAxis, yAxis,
        maybeDiag (gd p) grid, whiteRectB])
      p
  _ -> mempty


-- |Create the Diagram from a String which is supposed to be the contents
-- of an obj file.
diagS :: DiagProp -> MeshString -> Diagram Cairo R2
diagS p mesh =
  (diag p       .
      meshToArr $
      mesh) #
    bg white
