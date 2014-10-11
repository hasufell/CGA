{-# OPTIONS_HADDOCK ignore-exports #-}

module Algorithms.ConvexHull.GrahamScan where

import Algebra.Vector
import Algebra.VectorTypes
import Diagrams.TwoD.Types
import MyPrelude


-- |Get all points on a convex hull by using the graham scan
-- algorithm.
grahamGetCH :: [PT] -> [PT]
grahamGetCH vs =
        -- merge upper hull with lower hull while discarding
        -- the duplicated points from the lower hull
        f (reverse uH) uHRest ++ tailInit (f (reverse lH) lHRest)
  where
    -- sort lexicographically by x values (ties are resolved by y values)
    sortedVs = fmap p2 . sortLex . fmap unp2 $ vs
    -- lists for lower hull
    (lH, lHRest) = splitAt 2 sortedVs
    -- lists for upper hull
    (uH, uHRest) = splitAt 2 . reverse $ sortedVs
    -- This is the actual algorithm.
    -- If we have a list say:
    -- [(100, 100), (200, 450), (250, 250), (300, 400), (400, 200)]
    --
    -- then this will start with:
    -- [(200, 450), (100, 100)] and [(250, 250), (300, 400), (400, 200)]
    --
    -- The first list is reversed since we only care about the last
    -- 3 elements and want to stay efficient.
    f (y:z:xs) (x:ys)
      -- last 3 elements are ccw, but there are elements left to check
      |	ccw z y x   = f (x:y:z:xs) ys
      -- not ccw, pop one out
      | otherwise   = f (x:z:xs) ys
    f (x:y:z:xs) []
      -- nothing left and last 3 elements are ccw, so return
      |	ccw z y x   = x:y:z:xs
      -- not ccw, pop one out
      | otherwise   = f (x:z:xs) []
    f xs _ = xs


-- |Compute all steps of the graham scan algorithm to allow
-- visualizing it.
grahamGetCHSteps :: [PT] -> [[PT]]
grahamGetCHSteps vs =
  (++)  (reverse . g (length vs) (reverse lH) $ lHRest) .
    fmap (\x -> (last . reverse . g (length vs) (reverse lH) $ lHRest)
          ++ x) $
    (init . reverse . g (length vs) (reverse uH) $ uHRest)
  where
    sortedVs = fmap p2 . sortLex . fmap unp2 $ vs
    (lH, lHRest) = splitAt 2 sortedVs
    (uH, uHRest) = splitAt 2 . reverse $ sortedVs
    g c xs' ys'
      | c >= 0    = f 0 xs' ys' : g (c - 1) xs' ys'
      | otherwise = []
      where
        f c' (y:z:xs) (x:ys)
          | c' >= c     = reverse (y:z:xs)
          |	ccw z y x   = f (c' + 1) (x:y:z:xs) ys
          | otherwise   = f (c' + 1) (x:z:xs) ys
        f _ [x,y] []    = [y,x]
        f c' (x:y:z:xs) []
          | c' >= c     = reverse (x:y:z:xs)
          |	ccw z y x   = x:y:z:xs
          | otherwise   = f (c' + 1) (x:z:xs) []
        f _ xs _        = reverse xs
