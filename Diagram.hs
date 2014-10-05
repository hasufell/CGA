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
  -- |Get the thickness of the dot.
  t :: Double
}


-- |The default properties of the Diagram.
defaultProp :: DiagProp
defaultProp = MkProp 2


-- |Create the Diagram from the VTable.
diagFromVTable :: DiagProp -> VTable -> Diagram Cairo R2
diagFromVTable prop vt
  = position (zip (map mkPoint . filter (inRange 0 500) $ vt)
                  (repeat dot)) # moveTo (p2(-250, -250))
     `atop` hrule 500 # centerX # moveTo (p2(0, -250))
     `atop` vrule 500 # centerY # moveTo (p2(-250, 0))
     `atop` square 550 # lwG 0.00 # bg white
        where dot           = (circle $
                               t prop :: Diagram Cairo R2) # fc black
              mkPoint (x,y) = p2 (x,y)


-- |Create the Diagram from a String.
diagFromString :: DiagProp -> String -> Diagram Cairo R2
diagFromString prop mesh
  = diagFromVTable prop .
      meshToArr         $
      mesh
