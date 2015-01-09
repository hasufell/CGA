{-# OPTIONS_HADDOCK ignore-exports #-}

module Algebra.Polygon where

import Algebra.Vector
import Data.Maybe
import MyPrelude


-- |Split a polygon by a given segment which must be vertices of the
-- polygon (returns empty array otherwise).
splitPoly :: [PT]
          -> Segment
          -> [[PT]]
splitPoly pts (a, b)
  | elem a pts && elem b pts =
    [b : takeWhile (/= b) shiftedPoly, a : dropWhile (/= b) shiftedPoly]
  | otherwise = [[]]
    where
      shiftedPoly = shiftM' a pts


-- |Get all edges of a polygon.
polySegments :: [PT] -> [Segment]
polySegments p@(x':_:_:_) = go p ++ [(last p, x')]
  where
    go (x:y:xs) = (x, y) : go (y:xs)
    go _        = []
polySegments _ = []


-- |Check whether the given segment is inside the polygon.
-- This doesn't check for segments that are completely outside
-- of the polygon yet.
isInsidePoly :: [PT] -> Segment -> Bool
isInsidePoly pts seg =
  null
    . catMaybes
    . fmap (intersectSeg'' seg)
    $ polySegments pts


-- |Check whether two points are adjacent vertices of a polygon.
adjacent :: PT -> PT -> [PT] -> Bool
adjacent u v = any (\x -> x == (u, v) || x == (v, u)) . polySegments


-- |Check whether the polygon is a triangle polygon.
isTrianglePoly :: [PT] -> Bool
isTrianglePoly [_, _, _] = True
isTrianglePoly _         = False


-- |Get all triangle polygons.
triangleOnly :: [[PT]] -> [[PT]]
triangleOnly = filter isTrianglePoly


-- |Get all non-triangle polygons.
nonTriangleOnly :: [[PT]] -> [[PT]]
nonTriangleOnly = filter (not . isTrianglePoly)
