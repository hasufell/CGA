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


data Alignment = CW
               | CCW
               | CL
  deriving (Eq)


-- |Convert two dimensions such as (xmin, xmax) and (ymin, ymax)
-- to proper square coordinates, as in:
-- ((xmin, ymin), (xmax, ymax))
dimToSquare :: (Double, Double) -- ^ x dimension
            -> (Double, Double) -- ^ y dimension
            -> ((Double, Double), (Double, Double)) -- ^ square describing those dimensions
dimToSquare (x1, x2) (y1, y2) = ((x1, y1), (x2, y2))


-- |Checks whether the Point is in a given Square.
inRange :: ((Double, Double), (Double, Double)) -- ^ the square: ((xmin, ymin), (xmax, ymax))
        -> P2 Double  -- ^ Coordinate
        -> Bool       -- ^ result
inRange ((xmin, ymin), (xmax, ymax)) (coords -> x :& y)
  = x >= min xmin xmax
    && x <= max xmin xmax
    && y >= min ymin ymax
    && y <= max ymin ymax


-- |Get the angle between two vectors.
getAngle :: V2 Double -> V2 Double -> Double
getAngle a b =
  acos
    . flip (/) (vecLength a * vecLength b)
    . scalarProd a
    $ b


-- |Get the length of a vector.
vecLength :: V2 Double -> Double
vecLength v = sqrt (x^(2 :: Int) + y^(2 :: Int))
  where
    (x, y) = unr2 v


-- |Compute the scalar product of two vectors.
scalarProd :: V2 Double -> V2 Double -> Double
scalarProd (V2 a1 a2) (V2 b1 b2) = a1 * b1 + a2 * b2


-- |Multiply a scalar with a vector.
scalarMul :: Double -> V2 Double -> V2 Double
scalarMul d (V2 a b) = V2 (a * d) (b * d)


-- |Construct a vector that points to a point from the origin.
pt2Vec :: P2 Double -> V2 Double
pt2Vec = r2 . unp2


-- |Give the point which is at the coordinates the vector
-- points to from the origin.
vec2Pt :: V2 Double -> P2 Double
vec2Pt = p2 . unr2


-- |Construct a vector between two points.
vp2 :: P2 Double  -- ^ vector origin
    -> P2 Double  -- ^ vector points here
    -> V2 Double
vp2 a b = pt2Vec b - pt2Vec a


-- |Computes the determinant of 3 points.
det :: P2 Double -> P2 Double -> P2 Double -> Double
det (coords -> ax :& ay) (coords -> bx :& by) (coords -> cx :& cy) =
  (bx - ax) * (cy - ay) - (by - ay) * (cx - ax)


-- |Get the point where two lines intesect, if any.
intersectSeg' :: (P2 Double, P2 Double) -- ^ first segment
              -> (P2 Double, P2 Double) -- ^ second segment
              -> Maybe (P2 Double)
intersectSeg' (a, b) (c, d) =
  glossToPt <$> intersectSegSeg (ptToGloss a)
                                (ptToGloss b)
                                (ptToGloss c)
                                (ptToGloss d)
  where
    ptToGloss = (double2Float *** double2Float) <$> unp2
    glossToPt = p2 . (float2Double *** float2Double)


-- |Get the point where two lines intesect, if any. Excludes the
-- case of end-points intersecting.
intersectSeg'' :: (P2 Double, P2 Double) -> (P2 Double, P2 Double) -> Maybe (P2 Double)
intersectSeg'' (a, b) (c, d) = case intersectSeg' (a, b) (c, d) of
  Just x  -> if x `notElem` [a,b,c,d] then Just a else Nothing
  Nothing -> Nothing


-- |Get the orientation of 3 points which can either be
-- * clock-wise
-- * counter-clock-wise
-- * collinear
getOrient :: P2 Double -> P2 Double -> P2 Double -> Alignment
getOrient a b c = case compare (det a b c) 0 of
  LT -> CW
  GT -> CCW
  EQ -> CL


--- |Checks if 3 points a,b,c do not build a clockwise triangle by
--- connecting a-b-c. This is done by computing the determinant and
--- checking the algebraic sign.
notcw :: P2 Double -> P2 Double -> P2 Double -> Bool
notcw a b c = case getOrient a b c of
  CW -> False
  _  -> True


--- |Checks if 3 points a,b,c do build a clockwise triangle by
--- connecting a-b-c. This is done by computing the determinant and
--- checking the algebraic sign.
cw :: P2 Double -> P2 Double -> P2 Double -> Bool
cw a b c = not . notcw a b $ c


-- |Sort X and Y coordinates lexicographically.
sortedXY :: [P2 Double] -> [P2 Double]
sortedXY = fmap p2 . sortLex . fmap unp2


-- |Sort Y and X coordinates lexicographically.
sortedYX :: [P2 Double] -> [P2 Double]
sortedYX = fmap p2 . sortLexSwapped . fmap unp2


-- |Sort all points according to their X-coordinates only.
sortedX :: [P2 Double] -> [P2 Double]
sortedX xs =
  fmap p2
  . sortBy (\(a1, _) (a2, _) -> compare a1 a2)
  $ fmap unp2 xs


-- |Sort all points according to their Y-coordinates only.
sortedY :: [P2 Double] -> [P2 Double]
sortedY xs =
  fmap p2
  . sortBy (\(_, b1) (_, b2) -> compare b1 b2)
  $ fmap unp2 xs


-- |Apply a function on the coordinates of a point.
onPT :: ((Double, Double) -> (Double, Double)) -> P2 Double -> P2 Double
onPT f = p2 . f . unp2


-- |Compare the y-coordinate of two points.
ptCmpY :: P2 Double -> P2 Double -> Ordering
ptCmpY (coords -> _ :& y1) (coords -> _ :& y2) =
  compare y1 y2


-- |Compare the x-coordinate of two points.
ptCmpX :: P2 Double -> P2 Double -> Ordering
ptCmpX (coords -> x1 :& _) (coords -> x2 :& _) =
  compare x1 x2


posInfPT :: P2 Double
posInfPT = p2 (read "Infinity", read "Infinity")


negInfPT :: P2 Double
negInfPT = p2 (negate . read $ "Infinity", negate . read $ "Infinity")
