module Diagram where

import Diagrams.Prelude
import Diagrams.Backend.Cairo
import Meshparser
import Util


data DiagProp = MkProp {
  -- |Get the thickness of the dot.
  getThickness :: Double
}


-- |Create the Diagram from the VTable.
diagFromVTable :: DiagProp -> VTable -> Diagram Cairo R2
diagFromVTable prop meshArr
  = position (zip (map mkPoint . filter (inRange 0 500) $ meshArr)
                     (repeat dot)) # moveTo (p2(-250, -250))
   `atop` square 500 # lwG 0.05 # bg white
    where dot           = (circle $ getThickness prop :: Diagram Cairo R2) # fc black
          mkPoint (x,y) = p2 (x,y)

-- |Create the Diagram from a String.
diagFromString :: DiagProp -> String -> Diagram Cairo R2
diagFromString prop mesh
  = diagFromVTable prop .
      meshToArr         $
      mesh
