{-# OPTIONS_HADDOCK ignore-exports #-}

module Algorithms.ConvexHull.GrahamScan where

import Algebra.Vector
import Algebra.VectorTypes
import Diagrams.TwoD.Types
import MyPrelude


-- |Get all points on a convex hull by using the graham scan
-- algorithm.
{--
========== FUNCTIONAL PSEUDO CODE  ======================
input: unsorted list us'
output: sorted convex hull list

variables:
(lowerHull, restl) = splitAt3IntoTuple (sort us')
(upperHull, restu) = reverse (splitAt3IntoTuple (sort us'))

main scope:
return (scanHalf upperHull restu) ++
         (stripFirstAndLastElem(scanHalf lowerHull restl))

=== begin scanHalf function ===
scanHalf (min 3 elem => lowerHull) (min 1 elem => rest)
  | isCounterClockWise (last3Elements lowerHull) == True
    = scanHalf (lowerHull + head rest) (tail rest)
  | otherwise
    = scanHalf (deleteSndToLastElem lowerHull + head rest)
           (rest)

scanHalf (min 3 elem => lowerHull ) []
  | isCounterClockWise (last3Elements lowerHull) == True
    = return lowerHull
  | otherwise
    = scanHalf (deleteSndToLastElem lowerHull) []

scanHalf lowerHull (min 1 elem => rest) = scanHalf (lowerHull + head rest)
                                                   (tail rest)


scanHalf lowerHull _ = lowerHull
=== end scanHalf function ===


============= SIMULATION ===================================
xs = [(100, 100), (200, 450), (250, 250)]
ys = [(300, 400), (400, 200)]

ccw (100, 100) (200, 450) (250, 250) => false, pop snd2last of xs
===
move first of ys to end of xs

xs = [(100, 100), (250, 250), (300, 400)]
ys = [(400, 200)]

ccw (100, 100), (250, 250) (300, 400) => true
===
move first of ys to end of xs

xs = [(100, 100), (250, 250), (300, 400), (400, 200)]
ys = []

ccw (250, 250) (300, 400) (400, 200) => false, pop snd2last of xs
===
xs = [(100, 100), (250, 250), (400, 200)]
ys = []

ccw (100, 100) (250, 250) (400, 200) => false, pop snd2last of xs
===
xs = [(100, 100), (400, 200)]
ys = []
===
return [(100, 100), (400, 200)]
=========================================================
--}
grahamGetCH :: [PT] -> [PT]
grahamGetCH vs =
  scanH uH uHRest ++ tailInit (scanH lH lHRest)
  where
    sortedXY     = fmap p2 . sortLex . fmap unp2 $ vs
    (lH, lHRest) = first reverse . splitAt 3 $ sortedXY
    (uH, uHRest) = first reverse . splitAt 3 . reverse $ sortedXY
    -- This scans only a half of the convex hull. If it's the upper
    -- or lower half depends on the input.
    -- Also, the first list is reversed since we only care about the last
    -- 3 elements and want to stay efficient.
    scanH :: [PT] -- ^ the first 3 starting points in reversed order
          -> [PT] -- ^ the rest of the points
          -> [PT] -- ^ all convex hull points for the half
    scanH hs@(x:y:z:xs) (r':rs')
      |	ccw z y x     = scanH (r':hs) rs'
      | otherwise     = scanH (x:z:xs) (r':rs')
    scanH hs@(x:y:z:xs) []
      |	ccw z y x     = hs
      | otherwise     = scanH (x:z:xs) []
    scanH hs (r':rs') = scanH (r':hs) rs'
    scanH hs _        = hs


-- |Compute all steps of the graham scan algorithm to allow
-- visualizing it.
grahamGetCHSteps :: [PT] -> [[PT]]
grahamGetCHSteps vs =
  (++) (rmdups . reverse . g (length vs) lH $ lHRest)
       (rmdups . init . reverse . g (length vs) uH $ uHRest)
  where
    sortedXY = fmap p2 . sortLex . fmap unp2 $ vs
    (lH, lHRest) = first reverse . splitAt 3 $ sortedXY
    (uH, uHRest) = first reverse . splitAt 3 . reverse $ sortedXY
    g c xs' ys'
      | c >= 0    = scanH 0 xs' ys' : g (c - 1) xs' ys'
      | otherwise = []
      where
        scanH c' hs@(x:y:z:xs) (r':rs')
          | c' >= c      = hs
          |	ccw z y x    = scanH (c' + 1) (r':hs) rs'
          | otherwise    = scanH (c' + 1) (x:z:xs) (r':rs')
        scanH _ [x,y] [] = [y,x]
        scanH c' hs@(x:y:z:xs) []
          | c' >= c      = hs
          |	ccw z y x    = hs
          | otherwise    = scanH (c' + 1) (x:z:xs) []
        scanH c' hs (r':rs')
          | c' >= c      = hs
          | otherwise    = scanH (c' + 1) (r':hs) rs'
        scanH _ xs _     = xs
