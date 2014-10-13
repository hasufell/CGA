{-# OPTIONS_HADDOCK ignore-exports #-}

module Graphics.Diagram.Gif where

import Algebra.VectorTypes
import Codec.Picture.Gif
import Diagrams.Backend.Cairo
import Diagrams.Prelude
import Graphics.Diagram.Plotter
import Graphics.Diagram.Types
import Parser.Meshparser


-- |Return a list of tuples used by 'gifMain' to generate an animated gif.
gifDiag :: DiagProp -> [PT] -> [(Diagram Cairo R2, GifDelay)]
gifDiag p xs =
  fmap (\x -> (x, 100))                         .
    fmap (\x -> x <> nonChDiag)                 .
    flip (++)
      [mkDiag (convexHullPointsText `mappend`
        convexHullPoints)
        p xs <> lastUpperHull <> lastLowerHull] $
    (lowerHullList ++ ((<> lastLowerHull) <$> upperHullList))
  where
    upperHullList = convexHullLinesIntervalUpper p xs
    lastUpperHull = last upperHullList
    lowerHullList = convexHullLinesIntervalLower p xs
    lastLowerHull = last lowerHullList
    -- add the x-axis and the other default stuff
    nonChDiag =
      mconcat                      .
        fmap (\x -> mkDiag x p xs) $
        [coordPoints,
        xAxis,
        yAxis,
        grid,
        whiteRectB]


-- |Same as gifDiag, except that it takes a string containing the
-- mesh file content instead of the the points.
gifDiagS :: DiagProp -> MeshString -> [(Diagram Cairo R2, GifDelay)]
gifDiagS p = gifDiag p . meshToArr
