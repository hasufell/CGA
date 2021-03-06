module Algorithms.PolygonIntersection where


import           Algebra.Vector
import           Control.Applicative
import           Data.Dequeue (BankersDequeue)
import qualified Data.Dequeue           as Q
import           Data.List
import           Data.Maybe
import           Diagrams.TwoD.Types
import           MyPrelude
import           QueueEx


-- TODO: probably use a zipper.
-- |Describes a point on the convex hull of the polygon.
-- In addition to the point itself, both it's predecessor and
-- successor are saved for convenience.
data PolyPT =
  PolyA {
    id'   :: P2 Double
    , pre :: P2 Double
    , suc :: P2 Double
    }
  | PolyB {
    id'   :: P2 Double
    , pre :: P2 Double
    , suc :: P2 Double
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
sortLexPoly :: [P2 Double] -> [P2 Double]
sortLexPoly ps = maybe [] (`shiftM` ps) (elemIndex (yMax ps) ps)
  where
    yMax       = foldl1 (\x y -> if ptCmpY x y == GT then x else y)


-- |Make a PolyPT list out of a regular list of points, so
-- the predecessor and successors are all saved.
mkPolyPTList :: (P2 Double -> P2 Double -> P2 Double -> PolyPT) -- ^ PolyA or PolyB function
             -> [P2 Double]                       -- ^ polygon points
             -> [PolyPT]
mkPolyPTList f' pts@(x':y':_:_) =
  f' x' (last pts) y' : go f' pts
  where
    go f (x:y:z:xs) = f y x z : go f (y:z:xs)
    go f [x, y]     = [f y x x']
    go _ _          = []
mkPolyPTList _ _ = []


-- |Sort the points of two polygons according to their y-coordinates,
-- while saving the origin of that point. This is done in O(n).
sortLexPolys :: ([P2 Double], [P2 Double]) -> [PolyPT]
sortLexPolys (pA'@(_:_), pB'@(_:_)) =
  queueToList $ go (Q.fromList . mkPolyPTList PolyA . sortLexPoly $ pA')
                   (Q.fromList . mkPolyPTList PolyB . sortLexPoly $ pB')
  where
    -- Start recursive algorithm, each polygon is represented by a Queue.
    -- Traverse predecessor and successor and insert them in the right
    -- order into the resulting queue.
    -- We start at the max y-coordinates of both polygons.
    go :: BankersDequeue PolyPT -- polyA
       -> BankersDequeue PolyPT -- polyB
       -> BankersDequeue PolyPT -- sorted queue
    go pA pB
      -- Nothing to sort.
      | Q.null pA && Q.null pB = Q.empty
      -- Current point of polygon A is higher on the y-axis than the
      -- current point of polygon B, so insert it into the resulting
      -- queue and traverse the rest.
      | ptCmpY (fromMaybe negInfPT (id' <$> Q.first pA))
               (fromMaybe negInfPT (id' <$> Q.first pB)) == GT
        = Q.pushFront (go (maybeShift . snd . fromJust . Q.popFront $ pA) pB)
                      (fromJust . Q.first $ pA)
      -- Same as above, except that the current point of polygon B
      -- is higher.
      | otherwise = Q.pushFront (go pA (maybeShift . snd . fromJust . Q.popFront $ pB))
                                (fromJust . Q.first $ pB)

    -- Compare the first and the last element of the queue according
    -- to their y-coordinate and shift the queue (if necessary) so that
    -- the element with the highest value is at the front.
    maybeShift :: BankersDequeue PolyPT -> BankersDequeue PolyPT
    maybeShift q = if ptCmpY (fromMaybe posInfPT (id' <$> Q.first q))
                             (fromMaybe negInfPT (id' <$> Q.last q)) == GT
                     then q
                     else shiftQueueRight q
sortLexPolys _ = []


-- |Get all points that intersect between both polygons. This is done
-- in O(n).
intersectionPoints :: [PolyPT] -> [P2 Double]
intersectionPoints xs' = rmdups . go $ xs'
  where
    go [] = []
    go xs = (++) (segIntersections . scanLine $ xs)
                 (go (tail xs))

    -- Get the scan line or in other words the
    -- Segment pairs we are going to check for intersection.
    scanLine :: [PolyPT] -> ([(P2 Double, P2 Double)], [(P2 Double, P2 Double)])
    scanLine sp@(_:_) = (,) (getSegment isPolyA) (getSegment isPolyB)
      where
        getSegment f = fromMaybe []
                                 ((\x -> [(id' x, suc x), (id' x, pre x)])
                                   <$> (listToMaybe . filter f $ sp))
    scanLine _ = ([], [])

    -- Gets the actual intersections between the segments of
    -- both polygons we currently examine. This is done in O(1)
    -- since we have max 4 segments.
    segIntersections :: ([(P2 Double, P2 Double)], [(P2 Double, P2 Double)]) -> [P2 Double]
    segIntersections (a@(_:_), b@(_:_)) =
      catMaybes
      . fmap (\[(x1, y1), (x2, y2)] -> intersectSegSeg x1 y1 x2 y2)
      $ combinations a b
    segIntersections _ = []

    -- Gets all unique(!) combinations of two arrays. Both arrays
    -- are max 2, so this is actually O(1) for this algorithm.
    combinations :: [a] -> [a] -> [[a]]
    combinations xs ys = concat . fmap (\y -> fmap (\x -> [y, x]) xs) $ ys

