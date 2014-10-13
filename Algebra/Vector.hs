{-# OPTIONS_HADDOCK ignore-exports #-}

module Algebra.Vector where

import Algebra.VectorTypes
import Diagrams.TwoD.Types
import MyPrelude


-- |Checks whether the Point is in a given dimension.
inRange :: Coord -- ^ X dimension
        -> Coord -- ^ Y dimension
        -> PT    -- ^ Coordinates
        -> Bool  -- ^ result
inRange (xlD, xuD) (ylD, yuD) p = x <= xuD && x >= xlD && y <= yuD && y >= ylD
  where
    (x, y) = unp2 p


-- |Get the angle between two vectors.
getAngle :: Vec -> Vec -> Double
getAngle a b =
  acos                                   .
    flip (/) (vecLength a * vecLength b) .
    scalarProd a                         $
    b


-- |Get the length of a vector.
vecLength :: Vec -> Double
vecLength v = sqrt (x^(2 :: Int) + y^(2 :: Int))
  where
    (x, y) = unr2 v


-- |Compute the scalar product of two vectors.
scalarProd :: Vec -> Vec -> Double
scalarProd v1 v2 = a1 * b1 + a2 * b2
  where
    (a1, a2) = unr2 v1
    (b1, b2) = unr2 v2


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
det a b c =
  (bx - ax) *
    (cy - ay) -
    (by - ay) *
    (cx - ax)
  where
    (ax, ay) = unp2 a
    (bx, by) = unp2 b
    (cx, cy) = unp2 c


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


-- |Sort X and Y coordinates lexicographically.
sortedXY :: [PT] -> [PT]
sortedXY = fmap p2 . sortLex . fmap unp2
