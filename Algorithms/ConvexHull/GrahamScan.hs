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
        scan uH uHRest ++ tailInit (scan lH lHRest)
  where
    -- sort lexicographically by x values (ties are resolved by y values)
    sortedXY     = fmap p2 . sortLex . fmap unp2 $ vs
    -- lists for lower hull
    (lH, lHRest) = first reverse . splitAt 2 $ sortedXY
    -- lists for upper hull
    (uH, uHRest) = first reverse . splitAt 2 . reverse $ sortedXY
    -- This is the actual algorithm.
    -- If we have a list say:
    -- [(100, 100), (200, 450), (250, 250), (300, 400), (400, 200)]
    --
    -- then this will start with:
    -- [(200, 450), (100, 100)] and [(250, 250), (300, 400), (400, 200)]
    --
    -- The first list is reversed since we only care about the last
    -- 3 elements and want to stay efficient.
    scan :: [PT] -- ^ the starting convex hull points
         -> [PT] -- ^ the rest of the points
         -> [PT] -- ^ all convex hull points
    scan (y:z:xs) (x:ys)
      -- last 3 elements are ccw, but there are elements left to check
      |	ccw z y x   = scan (x:y:z:xs) ys
      -- not ccw, pop one out
      | otherwise   = scan (x:z:xs) ys
    scan (x:y:z:xs) []
      -- nothing left and last 3 elements are ccw, so return
      |	ccw z y x   = x:y:z:xs
      -- not ccw, pop one out
      | otherwise   = scan (x:z:xs) []
    scan xs _ = xs


-- |Compute all steps of the graham scan algorithm to allow
-- visualizing it.
grahamGetCHSteps :: [PT] -> [[PT]]
grahamGetCHSteps vs =
  (++) (rmdups . reverse . g (length vs) lH $ lHRest)
       (rmdups . init . reverse . g (length vs) uH $ uHRest)
  where
    sortedXY = fmap p2 . sortLex . fmap unp2 $ vs
    (lH, lHRest) = first reverse . splitAt 2 $ sortedXY
    (uH, uHRest) = first reverse . splitAt 2 . reverse $ sortedXY
    g c xs' ys'
      | c >= 0    = scan 0 xs' ys' : g (c - 1) xs' ys'
      | otherwise = []
      where
        scan c' (y:z:xs) (x:ys)
          | c' >= c     = y:z:xs
          |	ccw z y x   = scan (c' + 1) (x:y:z:xs) ys
          | otherwise   = scan (c' + 1) (x:z:xs) ys
        scan _ [x,y] []    = [y,x]
        scan c' (x:y:z:xs) []
          | c' >= c     = x:y:z:xs
          |	ccw z y x   = x:y:z:xs
          | otherwise   = scan (c' + 1) (x:z:xs) []
        scan _ xs _     = xs
