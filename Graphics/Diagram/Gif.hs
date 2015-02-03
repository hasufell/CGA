{-# OPTIONS_HADDOCK ignore-exports #-}

module Graphics.Diagram.Gif where

import Algebra.Vector(PT)
import Algorithms.GrahamScan
import Codec.Picture.Gif
import qualified Data.ByteString.Char8 as B
import Data.Monoid
import Diagrams.Backend.Cairo
import Diagrams.Prelude hiding ((<>))
import Graphics.Diagram.AlgoDiags
import Graphics.Diagram.Core
import Graphics.Diagram.Plotter
import Parser.Meshparser


-- |Return a list of tuples used by 'gifMain' to generate an animated gif.
gifDiag :: DiagProp -> [PT] -> [(Diagram Cairo R2, GifDelay)]
gifDiag p xs =
  fmap ((\x -> (x, 50)) . (<> nonChDiag))
   (upperHullList
     <> fmap (<> last upperHullList) lowerHullList
     <> [mkDiag (mconcat [convexHPText, convexHP, convexHLs])
         p{ showCoordText = True } [xs]])
  where
    upperHullList = mkGifDiag convexHStepsLs p purple grahamUHSteps xs
    lowerHullList = mkGifDiag convexHStepsLs p orange grahamLHSteps xs
    -- add the x-axis and the other default stuff
    nonChDiag =
      mconcat
        . fmap (\x -> mkDiag x p [xs])
        $ [coordPoints, plotterBG]


-- |Same as gifDiag, except that it takes a string containing the
-- mesh file content instead of the the points.
gifDiagS :: DiagProp -> B.ByteString -> [(Diagram Cairo R2, GifDelay)]
gifDiagS p = gifDiag p . filterValidPT p . meshVertices
