module Algorithms.PolygonIntersection.Core where


import           Algebra.Vector
import           Algebra.VectorTypes
import           Data.Dequeue (BankersDequeue)
import qualified Data.Dequeue           as Q
import           Data.List
import           Data.Maybe
import           Diagrams.TwoD.Types
import           MyPrelude
import           QueueEx
import           Safe


-- |Describes a point on the convex hull of the polygon.
-- In addition to the point itself, both it's predecessor and
-- successor are saved for convenience.
data PolyPT =
  PolyA {
    id'   :: PT
    , pre :: PT
    , suc :: PT
    }
  | PolyB {
    id'   :: PT
    , pre :: PT
    , suc :: PT
    }
  deriving (Show, Eq)


-- |Shift a list of sorted convex hull points of a polygon so that
-- the first element in the list is the one with the highest y-coordinate.
-- This is done in O(n).
sortLexPoly :: [PT] -> [PT]
sortLexPoly ps = maybe [] (`shiftM` ps) (elemIndex (yMax ps) ps)
  where
    yMax       = foldl1 (\x y -> if ptCmpY x y == GT then x else y)


-- | Sort the points of two polygons according to their y-coordinates,
-- while saving the origin of that point. This is done in O(n).
sortLexPolys :: ([PT], [PT]) -> [PolyPT]
sortLexPolys (pA'@(_:_), pB'@(_:_)) =
  queueToList . go (Q.fromList . sortLexPoly $ pA') $
                   (Q.fromList . sortLexPoly $ pB')
  where
    -- Start recursive algorithm, each polygon is represented by a Queue.
    -- Traverse predecessor and successor and insert them in the right
    -- order into the resulting queue.
    -- We start at the max y-coordinates of both polygons.
    go :: BankersDequeue PT -> BankersDequeue PT -> BankersDequeue PolyPT
    go pA pB
      -- Nothing to sort.
      | Q.null pA && Q.null pB
        = Q.empty
      -- Current point of polygon A is higher on the y-axis than the
      -- current point of polygon B, so insert it into the resulting
      -- queue and traverse the rest.
      -- remark: we don't handle y1 = y2
      | ptCmpY (fromMaybe negInfPT . Q.first $ pA)
               (fromMaybe posInfPT . Q.first $ pB) == GT
        = Q.pushFront
            (go (maybeShift . snd . Q.popFront $ pA) pB)
            (PolyA (fromJust . Q.first $ pA)
              (pre' pA' pA)
              (suc' pA' pA))

      -- Same as above, except that the current point of polygon B
      -- is higher.
      | otherwise
        = Q.pushFront
            (go pA (maybeShift . snd . Q.popFront $ pB))
            (PolyB (fromJust . Q.first $ pB)
              (pre' pB' pB)
              (suc' pB' pB))

    pre' xs = fromJust . polySuccessor xs . uQfirst
    suc' xs = fromJust . polyPredecessor xs . uQfirst

    -- Compare the first and the last element of the queue according
    -- to their y-coordinate and shift the queue (if necessary) so that
    -- the element with the highest value is at the front.
    maybeShift :: BankersDequeue PT -> BankersDequeue PT
    -- remark: we don't handle y1 = y2
    maybeShift q = if ptCmpY (fromMaybe posInfPT . Q.first $ q)
                             (fromMaybe negInfPT . Q.last  $ q) == GT
                     then q
                     else shiftQueueRight q
sortLexPolys _ = []


-- |Get the successor of a point on a convex hull of a polygon.
-- Returns Nothing if the point is not on the convex hull. This
-- is done in O(n).
polySuccessor :: [PT] -> PT -> Maybe PT
polySuccessor pts pt = case index of
  Nothing      -> Nothing
  Just index'  -> if index' == (length pts - 1)
                    then pts `atMay` 0
                    else pts `atMay` (index' + 1)
  where
    index = elemIndex pt pts


-- |Get the predecessor of a point on a convex hull of a polygon.
-- Returns Nothing if the point is not on the convex hull. This
-- is done in O(n).
polyPredecessor :: [PT] -> PT -> Maybe PT
polyPredecessor pts pt = case index of
  Nothing     -> Nothing
  Just index' -> if index' == 0
                   then pts `atMay` (length pts - 1)
                   else pts `atMay` (index' - 1)
  where
    index = elemIndex pt pts


-- |Get all points that intersect between both polygons. This is done
-- in O(n).
intersectionPoints :: [PolyPT] -> [PT]
intersectionPoints []  = []
intersectionPoints xs' =
  rmdups
    . (++) (segIntersections . scanLine $ xs')
    $ intersectionPoints (tail xs')
  where
    -- Get the scan line or in other words the
    -- Segment pairs we are going to check for intersection.
    scanLine :: [PolyPT] -> ([Segment], [Segment])
    scanLine xs = (segmentsA xs, sgementsB xs)

    -- Gets the actual intersections between the segments of
    -- both polygons we currently examine. This is done in O(1)
    -- since we have max 4 segments.
    segIntersections :: ([Segment], [Segment]) -> [PT]
    segIntersections (a@(_:_), b@(_:_))
      = catMaybes
          . fmap (\[x, y] -> intersectSeg' x y)
          $ combinations a b
    segIntersections _ = []

    -- Gets all unique(!) combinations of two arrays. Both arrays
    -- are max 2, so this is actually O(1) for this algorithm.
    combinations :: [a] -> [a] -> [[a]]
    combinations xs ys = concat . fmap (\y -> fmap (\x -> [y, x]) xs) $ ys

    segmentsA :: [PolyPT] -> [Segment]
    segmentsA sp@(_:_) = case a of
      Nothing -> []
      Just x  -> [(id' x, suc x), (id' x, pre x)]
      where
        a = listToMaybe . filter (\x -> case x of
                                           PolyA {} -> True
                                           _       -> False) $ sp
    segmentsA _ = []

    sgementsB :: [PolyPT] -> [Segment]
    sgementsB sp@(_:_) = case b of
      Nothing -> []
      Just x  -> [(id' x, suc x), (id' x, pre x)]
      where
        b = listToMaybe . filter (\x -> case x of
                                           PolyB {} -> True
                                           _       -> False) $ sp
    sgementsB _ = []


testArr :: ([PT], [PT])
testArr = ([p2 (200.0, 500.0),
            p2 (0.0, 200.0),
            p2 (200.0, 100.0),
            p2 (400.0, 300.0)],

           [p2 (350.0, 450.0),
            p2 (275.0, 225.0),
            p2 (350.0, 50.0),
            p2 (500.0, 0.0),
            p2 (450.0, 400.0)])
