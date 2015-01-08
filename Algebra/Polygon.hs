{-# OPTIONS_HADDOCK ignore-exports #-}

module Algebra.Polygon where

import Algebra.Vector
import MyPrelude
{- import Diagrams.Coordinates -}


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
