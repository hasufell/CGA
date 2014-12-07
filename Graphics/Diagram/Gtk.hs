{-# OPTIONS_HADDOCK ignore-exports #-}

module Graphics.Diagram.Gtk where

import Algebra.Vector(PT)
import qualified Data.ByteString.Char8 as B
import Data.List(find)
import Diagrams.Backend.Cairo
import Diagrams.Prelude
import Graphics.Diagram.AlgoDiags
import Graphics.Diagram.Core
import Graphics.Diagram.Plotter
import Parser.Meshparser


-- |Data structure that holds an algorithm identifier and it's
-- corresponding list of diagrams.
data DiagAlgo = DiagAlgo {
  algoNum :: Int,    -- the identifier for the algorithm
  getDiags :: [Diag] -- the diagrams making up this algorithm
}


-- |Introspective data structure holding all algorithms for the
-- coordinate system.
diagAlgos :: [DiagAlgo]
diagAlgos =
  [DiagAlgo 0 [coordPointsText, coordPoints, plotterBG]
  ,DiagAlgo 1 [convexHPText, convexHP, convexHLs, coordPoints, plotterBG]
  ,DiagAlgo 2 [polyLines, coordPointsText, coordPoints, plotterBG]
  ,DiagAlgo 3 [polyIntersectionText, polyIntersection,
                 coordPoints, polyLines, plotterBG]
  ,DiagAlgo 4 [quadPathSquare, squares, coordPointsText,
                 coordPoints, polyLines, plotterBG]
  ,DiagAlgo 5 [kdRange, kdSquares, coordPointsText, coordPoints, plotterBG]]


-- |Introspective data structure holding all algorithms for the
-- tree view.
diagTreAlgos :: [DiagAlgo]
diagTreAlgos =
  [DiagAlgo 4 [treePretty]
  ,DiagAlgo 5 [kdTreeDiag]]


-- |Create the Diagram from the points.
diag :: DiagProp -> [DiagAlgo] -> [[PT]] -> Diagram Cairo R2
diag p das vts = maybe mempty (\x -> mkDiag x p vts)
                  $ mconcat
                      <$> getDiags
                      <$> find (\(DiagAlgo x _) -> x == algo p) das


-- |Create the Diagram from a String which is supposed to be the contents
-- of an obj file.
diagS :: DiagProp -> B.ByteString -> Diagram Cairo R2
diagS p mesh
  | algo p == 2 || algo p == 3 =
      diag p diagAlgos . fmap (filterValidPT p) . facesToArr $ mesh
  | otherwise = diag p diagAlgos . (: []) . filterValidPT p . meshToArr $ mesh


-- |Create the tree diagram from a String which is supposed to be the contents
-- of an obj file.
diagTreeS :: DiagProp -> B.ByteString -> Diagram Cairo R2
diagTreeS p = diag p diagTreAlgos . (: []) . filterValidPT p . meshToArr
