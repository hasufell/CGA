{-# OPTIONS_HADDOCK ignore-exports #-}

module Graphics.Diagram.Gif where

import Algebra.VectorTypes
import Algorithms.ConvexHull.GrahamScan
import Codec.Picture.Gif
import Data.Monoid
import Diagrams.Backend.Cairo
import Diagrams.Prelude hiding ((<>))
import Graphics.Diagram.Plotter
import Graphics.Diagram.Types
import Parser.Meshparser


-- |Return a list of tuples used by 'gifMain' to generate an animated gif.
gifDiag :: DiagProp -> [PT] -> [(Diagram Cairo R2, GifDelay)]
gifDiag p xs =
  fmap ((\x -> (x, 50)) . (<> nonChDiag))
   (upperHullList
     <> fmap (<> last upperHullList) lowerHullList
     <> [mkDiag (mconcat [convexHPText, convexHP, convexHLs])
         p{ showCoordText = True } (Object xs)])
  where
    upperHullList = mkGifDiag convexHStepsLs p purple grahamUHSteps xs
    lowerHullList = mkGifDiag convexHStepsLs p orange grahamLHSteps xs
    -- add the x-axis and the other default stuff
    nonChDiag =
      mconcat                               .
        fmap (\x -> mkDiag x p (Object xs)) $
        [coordPoints, plotterBG]


-- |Same as gifDiag, except that it takes a string containing the
-- mesh file content instead of the the points.
gifDiagS :: DiagProp -> MeshString -> [(Diagram Cairo R2, GifDelay)]
gifDiagS p = gifDiag p . meshToArr
