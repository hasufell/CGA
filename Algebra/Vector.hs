{-# OPTIONS_HADDOCK ignore-exports #-}

module Algebra.Vector where

import Algebra.VectorTypes
import Control.Applicative
import Diagrams.TwoD.Types
import Graphics.Gloss.Geometry.Line
import GHC.Float
import MyPrelude


-- |Checks whether the Point is in a given dimension.
inRange :: Square -- ^ the square, defined by x/y dimensions
        -> PT     -- ^ Coordinate
        -> Bool   -- ^ result
inRange ((xlD, xuD), (ylD, yuD)) p =
  x <= xuD && x >= xlD && y <= yuD && y >= ylD
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


-- |Get the point where two lines intesect, if any.
intersectSeg' :: Segment -> Segment -> Maybe PT
intersectSeg' (a, b) (c, d) =
  glossToPt <$> intersectSegSeg (ptToGloss a)
                                (ptToGloss b)
                                (ptToGloss c)
                                (ptToGloss d)
  where
    ptToGloss = (\(x, y) -> (double2Float x, double2Float y)) <$> unp2
    glossToPt = p2 . (\(x, y) -> (float2Double x, float2Double y))


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


-- |Apply a function on the coordinates of a point.
onPT :: ((Double, Double) -> (Double, Double)) -> PT -> PT
onPT f = p2 . f . unp2


-- |Compare the y-coordinate of two points.
ptCmpY :: PT -> PT -> Ordering
ptCmpY p1' p2' = compare ((snd . unp2) p1') ((snd . unp2) p2')


-- |Compare the x-coordinate of two points.
ptCmpX :: PT -> PT -> Ordering
ptCmpX p1' p2' = compare ((fst . unp2) p1') ((fst . unp2) p2')


posInfPT :: PT
posInfPT = p2 (read "Infinity", read "Infinity")


negInfPT :: PT
negInfPT = p2 (negate . read $ "Infinity", negate . read $ "Infinity")
