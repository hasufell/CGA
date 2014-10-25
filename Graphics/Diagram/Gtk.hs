{-# OPTIONS_HADDOCK ignore-exports #-}

module Graphics.Diagram.Gtk where

import Algebra.VectorTypes
import Diagrams.Backend.Cairo
import Diagrams.Prelude
import Graphics.Diagram.Plotter
import Graphics.Diagram.Types
import Parser.Meshparser


-- |Create the Diagram from the points.
diag :: DiagProp -> [[PT]] -> Diagram Cairo R2
diag p pts = case alg p of
  0 ->
    mkDiag
      (mconcat [maybeDiag (ct p) coordPointsText,
        coordPoints, xAxis, yAxis,
        maybeDiag (gd p) grid, whiteRectB])
      p (head pts)
  1 ->
    mkDiag
      (mconcat
        [maybeDiag (ct p) convexHPText,
        convexHP, convexHLs,
        coordPoints, xAxis, yAxis,
        maybeDiag (gd p) grid, whiteRectB])
      p (head pts)
  2 -> polys
  3 ->
      polyIntText
      `atop`
      polyIntersection (head pts) (pts !! 1) p
      `atop`
      polys
  _ -> mempty
  where
    polys =
      mkDiag
        (mconcat [maybeDiag (ct p) coordPointsText, coordPoints, polyLines])
        p (head pts)
      `atop`
      mkDiag
        (mconcat
          [maybeDiag (ct p) coordPointsText,
          polyLines, coordPoints, xAxis, yAxis,
          maybeDiag (gd p) grid, whiteRectB])
        p (pts !! 1)
    polyIntText = if ct p
                    then polyIntersectionText (head pts) (pts !! 1) p
                    else mempty



-- |Create the Diagram from a String which is supposed to be the contents
-- of an obj file.
diagS :: DiagProp -> MeshString -> Diagram Cairo R2
diagS p mesh = case alg p of
  2 ->
    diag p.
      facesToArr $
      mesh
  3 ->
    diag p.
      facesToArr $
      mesh
  _ ->
    (diag p       .
        (:[])     .
        meshToArr $
        mesh) #
      bg white
