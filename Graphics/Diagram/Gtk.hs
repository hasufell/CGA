{-# OPTIONS_HADDOCK ignore-exports #-}

module Graphics.Diagram.Gtk where

import Diagrams.Backend.Cairo
import Diagrams.Prelude
import Graphics.Diagram.Plotter
import Graphics.Diagram.Types
import Parser.Meshparser


-- |Create the Diagram from the points.
diag :: DiagProp -> Object -> Diagram Cairo R2
diag p obj@(Object _)
  | alg p == 0 =
      mkDiag
        (mconcat [maybeDiag (ct p) coordPointsText,
          coordPoints, plotterBG])
        p obj
  | alg p == 1 =
      mkDiag
        (mconcat
          [maybeDiag (ct p) convexHPText,
          convexHP, convexHLs,
          coordPoints, plotterBG])
        p obj
  | otherwise = mempty
diag p objs@(Objects _)
  | alg p == 2 =
      mkDiag (mconcat [polyLines, maybeDiag (ct p) coordPointsText, coordPoints,
                      plotterBG])
        p objs
  | alg p == 3 =
      mkDiag (mconcat [maybeDiag (ct p) polyIntersectionText,
                      polyIntersection, maybeDiag (ct p) coordPointsText,
                      coordPoints, polyLines,
                      plotterBG])
        p objs
  | otherwise = mempty


-- |Create the Diagram from a String which is supposed to be the contents
-- of an obj file.
diagS :: DiagProp -> MeshString -> Diagram Cairo R2
diagS p mesh
  | alg p == 2 || alg p == 3 =
      diag p.
        Objects    .
        facesToArr $
        mesh
  | otherwise =
      (diag p       .
          Object    .
          meshToArr $
          mesh) #
        bg white
