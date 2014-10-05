module Diagram where

import Defaults
import Diagrams.Prelude
import Diagrams.Backend.Cairo
import Meshparser
import Util


instance Def DiagProp where
    def = defaultProp


-- |Holds the properties for a Diagram, like thickness of 2d points etc.
data DiagProp = MkProp {
  -- |The thickness of the dots.
  t :: Double,
  -- |The dimensions of the x-axis.
  dX :: (Double, Double),
  -- |The dimensions of the y-axis.
  dY :: (Double, Double)
}


-- |The default properties of the Diagram.
defaultProp :: DiagProp
defaultProp = MkProp 2 (0,500) (0,500)


-- |Create the Diagram from the VTable.
diagFromVTable :: DiagProp -> VTable -> Diagram Cairo R2
diagFromVTable prop vt
  = position (zip (map mkPoint . filter (inRange (dX prop) (dY prop)) $ vt)
                  (repeat dot))   # moveTo (p2(xOffset, yOffset))
     `atop` hrule (xuD - xlD)     # moveTo (p2(0, yOffset))
     `atop` vrule (yuD - ylD)     # moveTo (p2(xOffset, 0))
     `atop` emptyRect (xuD - xlD + 50) (yuD - ylD + 50)
        where dot           = (circle $
                               t prop :: Diagram Cairo R2) # fc black
              mkPoint (x,y) = p2 (x,y)
              xlD           = fst $ dX prop
              xuD           = snd $ dX prop
              ylD           = fst $ dY prop
              yuD           = snd $ dY prop
              -- 'Diagrams' sets (0,0) to be in the middle of the
              -- drawing area, so we need to shift it depending
              -- on the given dimensions.
              xOffset       = (negate xlD / 2) - (xuD / 2)
              yOffset       = (negate ylD / 2) - (yuD / 2)


-- |Create the Diagram from a String which is supposed to be the contents
-- of an obj file.
diagFromString :: DiagProp -> String -> Diagram Cairo R2
diagFromString prop mesh
  = diagFromVTable prop .
      meshToArr         $
      mesh


-- |Create a white rectangle with the given width and height.
emptyRect :: Double -> Double -> Diagram Cairo R2
emptyRect x y = rect x y # lwG 0.00 # bg white
