{-# OPTIONS_HADDOCK ignore-exports #-}

module Algebra.Polygon where

import Algebra.Vector
import Data.Maybe
import MyPrelude


splitPoly :: [PT]
          -> Segment
          -> [[PT]]
splitPoly pts (a, b)
  | elem a pts && elem b pts =
    [b : takeWhile (/= b) shiftedPoly, a : dropWhile (/= b) shiftedPoly]
  | otherwise = [[]]
    where
      shiftedPoly = shiftM' a pts


polySegments :: [PT] -> [Segment]
polySegments p@(x':_:_:_) = go p ++ [(last p, x')]
  where
    go (x:y:xs) = (x, y) : go (y:xs)
    go _        = []
polySegments _ = []


isInsidePoly :: [PT] -> Segment -> Bool
isInsidePoly pts seg =
  null
    . catMaybes
    . fmap (intersectSeg'' seg)
    $ polySegments pts


adjacent :: PT -> PT -> [PT] -> Bool
adjacent u v = any (\x -> x == (u, v) || x == (v, u)) . polySegments


isTrianglePoly :: [PT] -> Bool
isTrianglePoly [_, _, _] = True
isTrianglePoly _         = False


triangleOnly :: [[PT]] -> [[PT]]
triangleOnly = filter isTrianglePoly


nonTriangleOnly :: [[PT]] -> [[PT]]
nonTriangleOnly = filter (not . isTrianglePoly)
