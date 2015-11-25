{-# OPTIONS_HADDOCK ignore-exports #-}
{-# LANGUAGE ViewPatterns #-}

module Algebra.Vector where

import Control.Applicative
import Control.Arrow ((***))
import Data.List (sortBy)
import Diagrams.Coordinates
import Diagrams.TwoD.Types
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


-- |Get the point where two lines intesect, if any. Excludes the
-- case of end-points intersecting.
intersectSeg :: (P2 Double, P2 Double) -> (P2 Double, P2 Double) -> Maybe (P2 Double)
intersectSeg (a, b) (c, d) = case intersectSegSeg a b c d of
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



-- | Given an infinite line which intersects P1 and P2,
--      let P4 be the point on the line that is closest to P3.
--
--      Return an indication of where on the line P4 is relative to P1 and P2.
--
-- @
--      if P4 == P1 then 0
--      if P4 == P2 then 1
--      if P4 is halfway between P1 and P2 then 0.5
-- @
--
-- @
--        |
--       P1
--        |
--     P4 +---- P3
--        |
--       P2
--        |
-- @
--
{-# INLINE closestPointOnLineParam #-}
closestPointOnLineParam
        :: P2 Double        -- ^ `P1`
        -> P2 Double        -- ^ `P2`
        -> P2 Double        -- ^ `P3`
        -> Double

closestPointOnLineParam p1 p2 p3
        = pt2Vec (p3 - p1) `scalarProd` pt2Vec (p2 - p1)
        / pt2Vec (p2 - p1) `scalarProd` pt2Vec (p2 - p1)


-- | Given four points specifying two lines, get the point where the two lines
--   cross, if any. Note that the lines extend off to infinity, so the
--   intersection point might not line between either of the two pairs of points.
--
-- @
--     \\      /
--      P1  P4
--       \\ /
--        +
--       / \\
--      P3  P2
--     /     \\
-- @
--
intersectLineLine
        :: P2 Double        -- ^ `P1`
        -> P2 Double        -- ^ `P2`
        -> P2 Double        -- ^ `P3`
        -> P2 Double        -- ^ `P4`
        -> Maybe (P2 Double)

intersectLineLine (coords -> x1 :& y1)
                  (coords -> x2 :& y2)
                  (coords -> x3 :& y3)
                  (coords -> x4 :& y4)
 = let  dx12    = x1 - x2
        dx34    = x3 - x4

        dy12    = y1 - y2
        dy34    = y3 - y4

        den     = dx12 * dy34  - dy12 * dx34

   in if den == 0
        then Nothing
        else let
                det12   = x1*y2 - y1*x2
                det34   = x3*y4 - y3*x4

                numx    = det12 * dx34 - dx12 * det34
                numy    = det12 * dy34 - dy12 * det34
             in Just $ p2 (numx / den, numy / den)


-- | Get the point where a segment @P1-P2@ crosses another segement @P3-P4@,
--   if any.
intersectSegSeg
        :: P2 Double        -- ^ `P1`
        -> P2 Double        -- ^ `P2`
        -> P2 Double        -- ^ `P3`
        -> P2 Double        -- ^ `P4`
        -> Maybe (P2 Double)

intersectSegSeg p1 p2 p3 p4
        -- TODO: merge closest point checks with intersection, reuse subterms.
        | Just p0       <- intersectLineLine p1 p2 p3 p4
        , t12           <- closestPointOnLineParam p1 p2 p0
        , t23           <- closestPointOnLineParam p3 p4 p0
        , t12 >= 0 && t12 <= 1
        , t23 >= 0 && t23 <= 1
        = Just p0

        | otherwise
        = Nothing
