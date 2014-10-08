{-# OPTIONS_HADDOCK ignore-exports #-}

module Diagram (t,
                dX,
                dY,
                alg,
                defaultProp,
                diag,
                diagS,
                whiteRect) where

import Algorithms.ConvexHull
import Class.Defaults
import Diagrams.Prelude
import Diagrams.Backend.Cairo
import LinearAlgebra.Vector
import Parser.Meshparser


-- |Represents a Cairo Diagram. This allows us to create multiple
-- diagrams with different algorithms but based on the same
-- coordinates and common properties.
data Diag = Diag {
  mkDiag :: DiagProp
         -> [PT]
         -> Diagram Cairo R2
}


-- |Holds the properties for a Diagram, like thickness of 2d points etc.
data DiagProp = MkProp {
  -- |The thickness of the dots.
  t :: Double,
  -- |The dimensions of the x-axis.
  dX :: Coord,
  -- |The dimensions of the y-axis.
  dY :: Coord,
  -- |Algorithm to use.
  alg :: Int
}


instance Def DiagProp where
    def = defaultProp


instance Monoid Diag where
  mempty = Diag (\_ _ -> rect 0 0)
  mappend d1 d2 = Diag g
    where
      g p vt = mkDiag d1 p vt <> mkDiag d2 p vt
  mconcat = foldr mappend mempty


-- |The default properties of the Diagram.
defaultProp :: DiagProp
defaultProp = MkProp 2 (0,500) (0,500) 0


-- |Extract the lower bound of the x-axis dimension.
xlD :: DiagProp -> Double
xlD = fst . dX


-- |Extract the upper bound of the x-axis dimension.
xuD :: DiagProp -> Double
xuD = snd . dX


-- |Extract the lower bound of the y-axis dimension.
ylD :: DiagProp -> Double
ylD = fst . dY


-- |Extract the upper bound of the y-axis dimension.
yuD :: DiagProp -> Double
yuD = snd . dY



-- |Creates a Diagram that shows the coordinates from the VTable
-- as dots. The VTable and thickness of the dots can be controlled
-- via DiagProp.
showCoordinates :: Diag
showCoordinates = Diag f
  where
    f p vt
      = position (zip (filter (inRange (dX p) (dY p)) $ vt)
                      (repeat dot))
            where
              -- a dot itself is a diagram
              dot = (circle $ t p :: Diagram Cairo R2) # fc black


-- |Create a diagram which shows the points of the convex hull.
showConvexHullPoints :: Diag
showConvexHullPoints = Diag f
  where
    f p vt
      = position (zip (filter (inRange (dX p) (dY p)) $ vtch)
                      (repeat dot))
            where
              -- a dot itself is a diagram
              dot = (circle $ t p :: Diagram Cairo R2) # fc red # lc red
              vtch = grahamGetCH vt


-- |Creates a Diagram that shows an XAxis which is bound
-- by the dimensions given in xD from DiagProp.
showXAxis :: Diag
showXAxis = Diag f
  where
    f p _ = (strokeTrail . fromVertices $ [p2 (xlD p,0), p2 (xuD p, 0)]) # moveTo (p2 (xlD p,0))


-- |Creates a Diagram that shows an YAxis which is bound
-- by the dimensions given in yD from DiagProp.
showYAxis :: Diag
showYAxis = Diag f
  where
    f p _ = strokeTrail . fromVertices $ [p2 (0, ylD p), p2 (0, yuD p)] # moveTo (p2 (0, ylD p))


-- |Creates a Diagram that shows a white rectangle which is a little
-- bit bigger as both X and Y axis dimensions from DiagProp.
showWhiteRectB :: Diag
showWhiteRectB = Diag f
  where
    f p _ = whiteRect (w' + 50) (h' + 50) # moveTo (p2 (w' / 2, h' / 2))
      where
        w' = xuD p - xlD p
        h' = yuD p - ylD p


-- |Create the Diagram from the VTable.
diag :: DiagProp -> [PT] -> Diagram Cairo R2
diag p = case alg p of
  0 -> mkDiag
         (mconcat [showCoordinates, showXAxis, showYAxis, showWhiteRectB])
         p
  1 -> mkDiag
         (mconcat [showConvexHullPoints, showCoordinates,
                   showXAxis, showYAxis, showWhiteRectB])
         p
  _ -> mempty


-- |Create the Diagram from a String which is supposed to be the contents
-- of an obj file.
diagS :: DiagProp -> String -> Diagram Cairo R2
diagS p mesh
  = (diag p      .
      meshToArr $
      mesh) # bg white


-- |Create a white rectangle with the given width and height.
whiteRect :: Double -> Double -> Diagram Cairo R2
whiteRect x y = rect x y # lwG 0.00 # bg white
