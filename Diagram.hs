module Diagram where

import Diagrams.Prelude
import Diagrams.Backend.Cairo
import Meshparser
import Util


-- |Create the Diagram from the VTable.
diagFromVTable :: VTable -> Diagram Cairo R2
diagFromVTable meshArr
  = position (zip (map mkPoint . filterValidCoords 0 500 $ meshArr)
                     (repeat dot)) # moveTo (p2(-250, -250))
   `atop` square 500 # lwG 0.05 # bg white
    where dot           = (circle 2 :: Diagram Cairo R2) # fc black
          mkPoint (x,y) = p2 (x,y)

-- |Create the Diagram from a String.
diagFromString :: String -> Diagram Cairo R2
diagFromString mesh = diagFromVTable  .
  meshToArr                           $
  mesh
