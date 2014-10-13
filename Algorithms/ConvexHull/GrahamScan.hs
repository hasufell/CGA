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
  | isNotClockWise (last3Elements lowerHull) == True
    = scanHalf (lowerHull + head rest) (tail rest)
  | otherwise
    = scanHalf (deleteSndToLastElem lowerHull + head rest)
           (rest)

scanHalf (min 3 elem => lowerHull ) []
  | isNotClockWise (last3Elements lowerHull) == True
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

notcw (100, 100) (200, 450) (250, 250) => false, pop snd2last of xs
===
move first of ys to end of xs

xs = [(100, 100), (250, 250), (300, 400)]
ys = [(400, 200)]

notcw (100, 100), (250, 250) (300, 400) => true
===
move first of ys to end of xs

xs = [(100, 100), (250, 250), (300, 400), (400, 200)]
ys = []

notcw (250, 250) (300, 400) (400, 200) => false, pop snd2last of xs
===
xs = [(100, 100), (250, 250), (400, 200)]
ys = []

notcw (100, 100) (250, 250) (400, 200) => false, pop snd2last of xs
===
xs = [(100, 100), (400, 200)]
ys = []
===
return [(100, 100), (400, 200)]
=========================================================
--}
grahamCH :: [PT] -> [PT]
grahamCH vs = grahamUCH vs ++ (tailInit . grahamLCH $ vs)


-- |Get the lower part of the convex hull.
grahamLCH :: [PT] -> [PT]
grahamLCH vs = scanH lH lHRest
  where
    sortedXY     = fmap p2 . sortLex . fmap unp2 $ vs
    (lH, lHRest) = first reverse . splitAt 3 $ sortedXY


-- |Get the upper part of the convex hull.
grahamUCH :: [PT] -> [PT]
grahamUCH vs = scanH uH uHRest
  where
    sortedXY     = fmap p2 . sortLex . fmap unp2 $ vs
    (uH, uHRest) = first reverse . splitAt 3 . reverse $ sortedXY


-- |This scans only a half of the convex hull. If it's the upper
-- or lower half depends on the input.
-- Also, the first list is reversed since we only care about the last
-- 3 elements and want to stay efficient.
scanH :: [PT] -- ^ the first 3 starting points in reversed order
      -> [PT] -- ^ the rest of the points
      -> [PT] -- ^ all convex hull points for the half
scanH hs@(x:y:z:xs) (r':rs')
  |	notcw z y x     = scanH (r':hs) rs'
  | otherwise       = scanH (x:z:xs) (r':rs')
scanH hs@(x:y:z:xs) []
  |	notcw z y x     = hs
  | otherwise       = scanH (x:z:xs) []
scanH hs (r':rs')   = scanH (r':hs) rs'
scanH hs _          = hs


-- |Compute all steps of the graham scan algorithm to allow
-- visualizing it.
-- Whether the upper or lower hull is computed depends on the input.
grahamCHSteps :: Int -> [PT] -> [PT] -> [[PT]]
grahamCHSteps c xs' ys'
  | c >= 0    = scanH 0 xs' ys' : grahamCHSteps (c - 1) xs' ys'
  | otherwise = []
  where
    scanH c' hs@(x:y:z:xs) (r':rs')
      | c' >= c      = hs
      |	notcw z y x  = scanH (c' + 1) (r':hs) rs'
      | otherwise    = scanH (c' + 1) (x:z:xs) (r':rs')
    scanH c' hs@(x:y:z:xs) []
      | c' >= c      = hs
      |	notcw z y x  = hs
      | otherwise    = scanH (c' + 1) (x:z:xs) []
    scanH c' hs (r':rs')
      | c' >= c      = hs
      | otherwise    = scanH (c' + 1) (r':hs) rs'
    scanH _ xs _     = xs


-- |Get all iterations of the upper hull of the graham scan algorithm.
grahamUHSteps :: [PT] -> [[PT]]
grahamUHSteps vs =
  (++) [getLastX 2 sortedXY]                  .
    rmdups                                    .
    init                                      .
    reverse                                   .
    grahamCHSteps ((* 2) . length $ vs) uH $
    uHRest
  where
    sortedXY     = fmap p2 . sortLex . fmap unp2 $ vs
    (uH, uHRest) = first reverse . splitAt 3 . reverse $ sortedXY


-- |Get all iterations of the lower hull of the graham scan algorithm.
grahamLHSteps :: [PT] -> [[PT]]
grahamLHSteps vs =
  (++) [take 2 sortedXY]                      .
    rmdups                                    .
    reverse                                   .
    grahamCHSteps ((* 2) . length $ vs) lH $
    lHRest
  where
    sortedXY     = fmap p2 . sortLex . fmap unp2 $ vs
    (lH, lHRest) = first reverse . splitAt 3 $ sortedXY
