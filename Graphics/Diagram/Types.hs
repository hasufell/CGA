{-# OPTIONS_HADDOCK ignore-exports #-}

module Graphics.Diagram.Types where

import Algebra.Vector
import Algebra.VectorTypes
import Diagrams.Backend.Cairo
import Diagrams.Prelude
import MyPrelude


type MeshString = String


-- |Represents a Cairo Diagram. This allows us to create multiple
-- diagrams with different algorithms but based on the same
-- coordinates and common properties.
data Diag =
  Diag
  {
    mkDiag :: DiagProp
           -> Object
           -> Diagram Cairo R2
  }
  | GifDiag
  {
    mkGifDiag :: DiagProp
              -> Colour Double
              -> ([PT] -> [[PT]])
              -> [PT]
              -> [Diagram Cairo R2]
  }
  | EmptyDiag (Diagram Cairo R2)


data Object = Object [PT]
            | Objects [[PT]]


-- |Holds the properties for a Diagram, like thickness of 2d points etc.
-- This can also be seen as a context when merging multiple diagrams.
data DiagProp = MkProp {
  -- |The thickness of the dots.
  t :: Double,
  -- |The dimensions of the x-axis.
  dX :: Coord,
  -- |The dimensions of the y-axis.
  dY :: Coord,
  -- |Algorithm to use.
  alg :: Int,
  -- |If we want to show the grid.
  gd :: Bool,
  -- |If we want to show the coordinates as text.
  ct :: Bool,
  -- |Square size used to show the grid and x/y-axis.
  sqS :: Double
}


instance Def DiagProp where
  def = defaultProp


instance Monoid Diag where
  mempty = EmptyDiag mempty
  mappend d1@(Diag {}) d2@(Diag {}) = Diag g
    where
      g p obj = mkDiag d1 p obj <> mkDiag d2 p obj
  mappend d1@(GifDiag {}) d2@(Diag {}) = GifDiag g
    where
      g p col f vt = mkGifDiag d1 p col f vt ++ [mkDiag d2 p (Object vt)]
  mappend d1@(Diag {}) d2@(GifDiag {}) = GifDiag g
    where
      g p col f vt = mkDiag d2 p (Object vt) : mkGifDiag d1 p col f vt
  mappend d1@(GifDiag {}) d2@(GifDiag {}) = GifDiag g
    where
      g p col f vt = mkGifDiag d1 p col f vt ++ mkGifDiag d2 p col f vt
  mappend (EmptyDiag _) g = g
  mappend g (EmptyDiag _) = g

  mconcat = foldr mappend mempty


-- |The default properties of the Diagram.
defaultProp :: DiagProp
defaultProp = MkProp 2 (0,500) (0,500) 0 False False 50


-- |Extract the lower bound of the x-axis dimension.
xmin :: DiagProp -> Double
xmin = fst . dX


-- |Extract the upper bound of the x-axis dimension.
xmax :: DiagProp -> Double
xmax = snd . dX


-- |Extract the lower bound of the y-axis dimension.
ymin :: DiagProp -> Double
ymin = fst . dY


-- |Extract the upper bound of the y-axis dimension.
ymax :: DiagProp -> Double
ymax = snd . dY


-- |The full width of the x dimension.
w' :: DiagProp -> Double
w' p = xmax p - xmin p


-- |The full height of the y dimension.
h' :: DiagProp -> Double
h' p = ymax p - ymin p


-- |The offset on the x-axis to move the grid and the white rectangle
-- to the right place.
wOff :: DiagProp -> Double
wOff p = xmin p + (w' p / 2)


-- |The offset on the y-axis to move the grid and the white rectangle
-- to the right place.
hOff :: DiagProp -> Double
hOff p = ymin p + (h' p / 2)


-- |Returns the specified diagram if True is passed,
-- otherwise returns the empty diagram. This is just for convenience
-- to avoid if else constructs.
maybeDiag :: Bool -> Diag -> Diag
maybeDiag b d
  | b         = d
  | otherwise = mempty


filterValidPT :: DiagProp -> [PT] -> [PT]
filterValidPT p = filter (inRange (dX p, dY p))
