module Algorithms.PolygonIntersection.Core where


import           Algebra.Vector
import           Algebra.VectorTypes
import           Control.Applicative
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


isPolyA :: PolyPT -> Bool
isPolyA PolyA {} = True
isPolyA _        = False


isPolyB :: PolyPT -> Bool
isPolyB = not . isPolyA


-- |Shift a list of sorted convex hull points of a polygon so that
-- the first element in the list is the one with the highest y-coordinate.
-- This is done in O(n).
sortLexPoly :: [PT] -> [PT]
sortLexPoly ps = maybe [] (`shiftM` ps) (elemIndex (yMax ps) ps)
  where
    yMax       = foldl1 (\x y -> if ptCmpY x y == GT then x else y)


-- |Sort the points of two polygons according to their y-coordinates,
-- while saving the origin of that point. This is done in O(n).
sortLexPolys :: ([PT], [PT]) -> [PolyPT]
sortLexPolys (pA'@(_:_), pB'@(_:_)) =
  queueToList $ go (Q.fromList . sortLexPoly $ pA')
                   (Q.fromList . sortLexPoly $ pB')
  where
    -- Start recursive algorithm, each polygon is represented by a Queue.
    -- Traverse predecessor and successor and insert them in the right
    -- order into the resulting queue.
    -- We start at the max y-coordinates of both polygons.
    go :: BankersDequeue PT -> BankersDequeue PT -> BankersDequeue PolyPT
    go pA pB
      -- Nothing to sort.
      | Q.null pA && Q.null pB = Q.empty
      -- Current point of polygon A is higher on the y-axis than the
      -- current point of polygon B, so insert it into the resulting
      -- queue and traverse the rest.
      | ptCmpY (fromMaybe negInfPT . Q.first $ pA)
               (fromMaybe posInfPT . Q.first $ pB) == GT
        = Q.pushFront (go (maybeShift . snd . Q.popFront $ pA) pB)
                      (mkPolyPT PolyA pA' pA)
      -- Same as above, except that the current point of polygon B
      -- is higher.
      | otherwise = Q.pushFront (go pA (maybeShift . snd . Q.popFront $ pB))
                                (mkPolyPT PolyB pB' pB)

    mkPolyPT f xs qs = f (fromJust . Q.first $ qs)
                         (getPT' polySuccessor xs qs)
                         (getPT' polyPredecessor xs qs)
      where
        getPT' f' xs' = fromJust . f' xs' . uQfirst

    -- Compare the first and the last element of the queue according
    -- to their y-coordinate and shift the queue (if necessary) so that
    -- the element with the highest value is at the front.
    maybeShift :: BankersDequeue PT -> BankersDequeue PT
    maybeShift q = if ptCmpY (fromMaybe posInfPT . Q.first $ q)
                             (fromMaybe negInfPT . Q.last  $ q) == GT
                     then q
                     else shiftQueueRight q
sortLexPolys _ = []


-- |Get the successor of a point on a convex hull of a polygon.
-- Returns Nothing if the point is not on the convex hull. This
-- is done in O(n).
polySuccessor :: [PT] -> PT -> Maybe PT
polySuccessor pts = polyPreSucInternal (length pts - 1, 0, 1) pts


-- |Get the predecessor of a point on a convex hull of a polygon.
-- Returns Nothing if the point is not on the convex hull. This
-- is done in O(n).
polyPredecessor :: [PT] -> PT -> Maybe PT
polyPredecessor pts = polyPreSucInternal (0, length pts - 1, negate 1) pts


-- |Abstraction for polyPredecessor and polySuccessor.
polyPreSucInternal :: (Int, Int, Int) -> [PT] -> PT -> Maybe PT
polyPreSucInternal (i1, i2, i3) pts pt = case index of
  Nothing     -> Nothing
  Just index' -> if index' == i1
                   then pts `atMay` i2
                   else pts `atMay` (index' + i3)
  where
    index = elemIndex pt pts


-- |Get all points that intersect between both polygons. This is done
-- in O(n).
intersectionPoints :: [PolyPT] -> [PT]
intersectionPoints []  = []
intersectionPoints xs' =
  rmdups $
    (++) (segIntersections . scanLine $ xs')
         (intersectionPoints (tail xs'))
  where
    -- Get the scan line or in other words the
    -- Segment pairs we are going to check for intersection.
    scanLine :: [PolyPT] -> ([Segment], [Segment])
    scanLine sp@(_:_) = (,) (getSegment isPolyA) (getSegment isPolyB)
      where
        getSegment f = fromMaybe []
                                 ((\x -> [(id' x, suc x), (id' x, pre x)])
                                   <$> (listToMaybe . filter f $ sp))
    scanLine _ = ([], [])

    -- Gets the actual intersections between the segments of
    -- both polygons we currently examine. This is done in O(1)
    -- since we have max 4 segments.
    segIntersections :: ([Segment], [Segment]) -> [PT]
    segIntersections (a@(_:_), b@(_:_)) =
      catMaybes
        . fmap (\[x, y] -> intersectSeg' x y)
        $ combinations a b
    segIntersections _ = []

    -- Gets all unique(!) combinations of two arrays. Both arrays
    -- are max 2, so this is actually O(1) for this algorithm.
    combinations :: [a] -> [a] -> [[a]]
    combinations xs ys = concat . fmap (\y -> fmap (\x -> [y, x]) xs) $ ys


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
