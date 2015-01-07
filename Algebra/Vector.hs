{-# OPTIONS_HADDOCK ignore-exports #-}
{-# LANGUAGE ViewPatterns #-}

module Algebra.Vector where

import Control.Applicative
import Control.Arrow ((***))
import Data.List (sortBy)
import Diagrams.Coordinates
import Diagrams.TwoD.Types
import Graphics.Gloss.Geometry.Line
import GHC.Float
import MyPrelude


type Vec     = R2
type PT      = P2
type Coord   = (Double, Double)
type Segment = (PT, PT)
type Square  = (Coord, Coord)


data Alignment = CW
               | CCW
               | CL
  deriving (Eq)


-- |Convert two dimensions such as (xmin, xmax) and (ymin, ymax)
-- to proper square coordinates, as in:
-- ((xmin, ymin), (xmax, ymax))
dimToSquare :: (Double, Double) -- ^ x dimension
            -> (Double, Double) -- ^ y dimension
            -> Square           -- ^ square describing those dimensions
dimToSquare (x1, x2) (y1, y2) = ((x1, y1), (x2, y2))


-- |Checks whether the Point is in a given Square.
inRange :: Square -- ^ the square: ((xmin, ymin), (xmax, ymax))
        -> PT     -- ^ Coordinate
        -> Bool   -- ^ result
inRange ((xmin, ymin), (xmax, ymax)) (coords -> x :& y)
  = x >= min xmin xmax
    && x <= max xmin xmax
    && y >= min ymin ymax
    && y <= max ymin ymax


-- |Get the angle between two vectors.
getAngle :: Vec -> Vec -> Double
getAngle a b =
  acos
    . flip (/) (vecLength a * vecLength b)
    . scalarProd a
    $ b


-- |Get the length of a vector.
vecLength :: Vec -> Double
vecLength v = sqrt (x^(2 :: Int) + y^(2 :: Int))
  where
    (x, y) = unr2 v


-- |Compute the scalar product of two vectors.
scalarProd :: Vec -> Vec -> Double
scalarProd (R2 a1 a2) (R2 b1 b2) = a1 * b1 + a2 * b2


-- |Multiply a scalar with a vector.
scalarMul :: Double -> Vec -> Vec
scalarMul d (R2 a b) = R2 (a * d) (b * d)


-- |Construct a vector that points to a point from the origin.
pt2Vec :: PT -> Vec
pt2Vec = r2 . unp2


-- |Give the point which is at the coordinates the vector
-- points to from the origin.
vec2Pt :: Vec -> PT
vec2Pt = p2 . unr2


-- |Construct a vector between two points.
vp2 :: PT  -- ^ vector origin
    -> PT  -- ^ vector points here
    -> Vec
vp2 a b = pt2Vec b - pt2Vec a


-- |Computes the determinant of 3 points.
det :: PT -> PT -> PT -> Double
det (coords -> ax :& ay) (coords -> bx :& by) (coords -> cx :& cy) =
  (bx - ax) * (cy - ay) - (by - ay) * (cx - ax)


-- |Get the point where two lines intesect, if any.
intersectSeg' :: Segment -> Segment -> Maybe PT
intersectSeg' (a, b) (c, d) =
  glossToPt <$> intersectSegSeg (ptToGloss a)
                                (ptToGloss b)
                                (ptToGloss c)
                                (ptToGloss d)
  where
    ptToGloss = (double2Float *** double2Float) <$> unp2
    glossToPt = p2 . (float2Double *** float2Double)


-- |Get the orientation of 3 points which can either be
-- * clock-wise
-- * counter-clock-wise
-- * collinear
getOrient :: PT -> PT -> PT -> Alignment
getOrient a b c = case compare (det a b c) 0 of
  LT -> CW
  GT -> CCW
  EQ -> CL


--- |Checks if 3 points a,b,c do not build a clockwise triangle by
--- connecting a-b-c. This is done by computing the determinant and
--- checking the algebraic sign.
notcw :: PT -> PT -> PT -> Bool
notcw a b c = case getOrient a b c of
  CW -> False
  _  -> True


--- |Checks if 3 points a,b,c do build a clockwise triangle by
--- connecting a-b-c. This is done by computing the determinant and
--- checking the algebraic sign.
cw :: PT -> PT -> PT -> Bool
cw a b c = not . notcw a b $ c


-- |Sort X and Y coordinates lexicographically.
sortedXY :: [PT] -> [PT]
sortedXY = fmap p2 . sortLex . fmap unp2


-- |Sort all points according to their X-coordinates only.
sortedX :: [PT] -> [PT]
sortedX xs =
  fmap p2
  . sortBy (\(a1, _) (a2, _) -> compare a1 a2)
  $ fmap unp2 xs


-- |Sort all points according to their Y-coordinates only.
sortedY :: [PT] -> [PT]
sortedY xs =
  fmap p2
  . sortBy (\(_, b1) (_, b2) -> compare b1 b2)
  $ fmap unp2 xs


-- |Apply a function on the coordinates of a point.
onPT :: (Coord -> Coord) -> PT -> PT
onPT f = p2 . f . unp2


-- |Compare the y-coordinate of two points.
ptCmpY :: PT -> PT -> Ordering
ptCmpY (coords -> _ :& y1) (coords -> _ :& y2) =
  compare y1 y2


-- |Compare the x-coordinate of two points.
ptCmpX :: PT -> PT -> Ordering
ptCmpX (coords -> x1 :& _) (coords -> x2 :& _) =
  compare x1 x2


posInfPT :: PT
posInfPT = p2 (read "Infinity", read "Infinity")


negInfPT :: PT
negInfPT = p2 (negate . read $ "Infinity", negate . read $ "Infinity")
