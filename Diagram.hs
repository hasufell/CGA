{-# OPTIONS_HADDOCK ignore-exports #-}

module Diagram (t,
                dX,
                dY,
                alg,
                defaultProp,
                diag,
                diagS,
                whiteRect) where

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
         -> VTable
         -> Diagram Cairo R2
}


-- |Holds the properties for a Diagram, like thickness of 2d points etc.
data DiagProp = MkProp {
  -- |The thickness of the dots.
  t :: Double,
  -- |The dimensions of the x-axis.
  dX :: (Double, Double),
  -- |The dimensions of the y-axis.
  dY :: (Double, Double),
  -- |Algorithm to use.
  alg :: Int
}


instance Def DiagProp where
    def = defaultProp


instance Monoid Diag where
  mempty = Diag (\_ _ -> rect 0 0 # lwG 0.00)
  mappend d1 d2 = Diag g
    where
      g p vt = mkDiag d1 p vt `atop` mkDiag d2 p vt
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


-- |The X offset to move coordinates to the right
-- position depending on the X dimensions.
xOffset :: DiagProp -> Double
xOffset p = (negate (xlD p) / 2) - (xuD p / 2)


-- |The Y offset to move coordinates to the right
-- position depending on the X dimensions.
yOffset :: DiagProp -> Double
yOffset p = (negate (ylD p) / 2) - (yuD p / 2)


-- |Creates a Diagram that shows the coordinates from the VTable
-- as dots. The VTable and thickness of the dots can be controlled
-- via DiagProp.
showCoordinates :: Diag
showCoordinates = Diag f
  where
    f p vt
      = position (zip (map mkPoint . filter (inRange (dX p) (dY p)) $ vt)
                      (repeat dot))   # moveTo (p2(xOffset p, yOffset p))
            where
              -- a dot itself is a diagram
              dot = (circle $ t p :: Diagram Cairo R2) # fc black
              -- this is just abstraction
              mkPoint (x,y) = p2 (x,y)


-- |Creates a Diagram that shows an XAxis which is bound
-- by the dimensions given in xD from DiagProp.
showXAxis :: Diag
showXAxis = Diag f
  where
    f p _ = hrule (xuD p - xlD p) # moveTo (p2(0, yOffset p))


-- |Creates a Diagram that shows an YAxis which is bound
-- by the dimensions given in yD from DiagProp.
showYAxis :: Diag
showYAxis = Diag f
  where
    f p _ = vrule (yuD p - ylD p) # moveTo (p2(xOffset p, 0))


-- |Creates a Diagram that shows a white rectangle which is a little
-- bit bigger as both X and Y axis dimensions from DiagProp.
showWhiteRectB :: Diag
showWhiteRectB = Diag f
  where
    f p _ = whiteRect (xuD p - xlD p + 50) (yuD p - ylD p + 50)


-- |Create the Diagram from the VTable.
diag :: DiagProp -> VTable -> Diagram Cairo R2
diag p = case alg p of
  0 -> mkDiag
         (mconcat [showCoordinates, showXAxis, showYAxis, showWhiteRectB])
         p
  _ -> mempty


-- |Create the Diagram from a String which is supposed to be the contents
-- of an obj file.
diagS :: DiagProp -> String -> Diagram Cairo R2
diagS p mesh
  = diag p     .
      meshToArr $
      mesh


-- |Create a white rectangle with the given width and height.
whiteRect :: Double -> Double -> Diagram Cairo R2
whiteRect x y = rect x y # lwG 0.00 # bg white
