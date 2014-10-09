{-# OPTIONS_HADDOCK ignore-exports #-}

module Algorithms.ConvexHull where

import Data.List
import Diagrams.TwoD.Types
import Diagrams.TwoD.Vector
import Util
import LinearAlgebra.Vector


-- |Find the point with the lowest Y coordinate.
-- If the lowest y-coordinate exists in more than one point in the set,
-- the point with the lowest x-coordinate out of the candidates is
-- chosen.
lowestYC :: [PT] -> PT
lowestYC []          = error "lowestYC: empty list"
lowestYC [a]         = a
lowestYC (a:b:vs)
  | ay >  by     = lowestYC (b:vs)
  | ay == by &&
    ax >  bx     = lowestYC (b:vs)
  | otherwise    = lowestYC (a:vs)
    where
      (ax, ay) = unp2 a
      (bx, by) = unp2 b


-- |Sort the points in increasing order of their degree between
-- P0 and the x-axis.
grahamSort :: [PT] -- ^ the points to sort
           -> [PT] -- ^ sorted points
grahamSort [] = []
grahamSort xs = p0 : sortBy (\a b
  -> noEqual a b                            .
       compare
       (getAngle (pt2Vec a - pt2Vec p0) xv) $
       (getAngle (pt2Vec b - pt2Vec p0) xv))
  (removeItem p0 xs)
    where
      xv = unitX
      p0 = lowestYC xs
      -- Have to account for corner cases when points are in
      -- a straight line or have the same y coordinates. Eq is
      -- not an option anyhow.
      noEqual :: PT -> PT -> Ordering -> Ordering
      noEqual a b EQ
        | ay == by &&
          ax < bx      = LT
        | otherwise    = GT
          where
            (ax, ay) = unp2 a
            (bx, by) = unp2 b
      noEqual _ _ LT     = LT
      noEqual _ _ GT     = GT


-- |Get all points on a convex hull by using the graham scan
-- algorithm.
grahamGetCH :: [PT] -> [PT]
grahamGetCH vs = f . grahamSort $ vs
  where
    f (x:y:z:xs)
      | ccw x y z = x : f (y:z:xs)
      | otherwise = f (x:z:xs)
    f xs          = xs


-- |Compute all steps of the graham scan algorithm to allow
-- visualizing it.
grahamGetCHSteps :: [PT] -> [[PT]]
grahamGetCHSteps vs = reverse . g $ (length vs - 2)
  where
    vs' = grahamSort vs
    g c
      | c >= 0 = f 0 vs' : g (c - 1)
      | otherwise = []
      where
        f c' (x:y:z:xs)
          | c' >= c   = [x,y]
          | ccw x y z = x : f (c' + 1) (y:z:xs)
          | otherwise = f (c' + 1) (x:z:xs)
        f _ xs        = xs
