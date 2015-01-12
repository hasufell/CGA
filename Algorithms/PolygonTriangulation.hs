{-# OPTIONS_HADDOCK ignore-exports #-}

module Algorithms.PolygonTriangulation where

import Algebra.Polygon
import Algebra.Vector
import qualified Control.Arrow as A
import Data.Maybe
import Safe


data VCategory = VStart
               | VEnd
               | VRegular
               | VSplit
               | VMerge
  deriving (Show, Eq)


-- |Classify all vertices on a polygon into five categories (see VCategory).
classifyList :: [PT] -> [(PT, VCategory)]
classifyList p@(x:y:_:_) =
  -- need to handle the first and last element separately
  [classify (last p) x y] ++ go p ++ [classify (last . init $ p) (last p) x]
  where
    go :: [PT] -> [(PT, VCategory)]
    go (x':y':z':xs) = classify x' y' z' : go (y':z':xs)
    go _          = []
classifyList _ = []


-- |Classify a vertex on a polygon given it's next and previous vertex
-- into five categories (see VCategory).
classify :: PT  -- ^ prev vertex
         -> PT  -- ^ classify this one
         -> PT  -- ^ next vertex
         -> (PT, VCategory)
classify prev v next
  | isVStart prev v next = (v, VStart)
  | isVSplit prev v next = (v, VSplit)
  | isVEnd prev v next   = (v, VEnd)
  | isVMerge prev v next = (v, VMerge)
  | otherwise            = (v, VRegular)


-- |Whether the vertex, given it's next and previous vertex,
-- is a start vertex.
isVStart :: PT  -- ^ previous vertex
         -> PT  -- ^ vertice to check
         -> PT  -- ^ next vertex
         -> Bool
isVStart prev v next =
  ptCmpY next v == LT && ptCmpY prev v == LT && cw next v prev


-- |Whether the vertex, given it's next and previous vertex,
-- is a split vertex.
isVSplit :: PT  -- ^ previous vertex
         -> PT  -- ^ vertice to check
         -> PT  -- ^ next vertex
         -> Bool
isVSplit prev v next =
  ptCmpY prev v == LT && ptCmpY next v == LT && cw prev v next


-- |Whether the vertex, given it's next and previous vertex,
-- is an end vertex.
isVEnd :: PT  -- ^ previous vertex
       -> PT  -- ^ vertice to check
       -> PT  -- ^ next vertex
       -> Bool
isVEnd prev v next =
  ptCmpY prev v == GT && ptCmpY next v == GT && cw next v prev


-- |Whether the vertex, given it's next and previous vertex,
-- is a merge vertex.
isVMerge :: PT  -- ^ previous vertex
         -> PT  -- ^ vertice to check
         -> PT  -- ^ next vertex
         -> Bool
isVMerge prev v next =
  ptCmpY next v == GT && ptCmpY prev v == GT && cw prev v next


-- |Whether the vertex, given it's next and previous vertex,
-- is a regular vertex.
isVRegular :: PT  -- ^ previous vertex
           -> PT  -- ^ vertice to check
           -> PT  -- ^ next vertex
           -> Bool
isVRegular prev v next =
     (not . isVStart prev v $ next)
  && (not . isVSplit prev v $ next)
  && (not . isVEnd prev v $ next)
  && (not . isVMerge prev v $ next)



-- |A polygon P is y-monotone, if it has no split and merge vertices.
isYmonotone :: [PT] -> Bool
isYmonotone poly =
  not
  . any (\x -> x == VSplit || x == VMerge)
  . fmap snd
  $ classifyList poly


-- |Partition P in y-monotone pieces.
monotonePartitioning :: [PT] -> [[PT]]
monotonePartitioning pts
  | isYmonotone pts = [pts]
  | otherwise = go (monotoneDiagonals pts) pts
  where
    go :: [Segment] -> [PT] -> [[PT]]
    go [] _ = [[]]
    go _ [] = [[]]
    go (x:xs) pts'
      | isYmonotone a && isYmonotone b = [a, b]
      | isYmonotone b = b : go xs a
      | otherwise = a : go xs b
      where
        [a, b] = splitPoly pts' x


-- |Try to eliminate the merge and split vertices by computing the
-- diagonals we have to use for splitting the polygon. This doesn't
-- necessarily make our polygon y-monotone yet.
monotoneDiagonals :: [PT] -> [Segment]
monotoneDiagonals pts = catMaybes . go $ classifyList pts
  where
    go :: [(PT, VCategory)] -> [Maybe Segment]
    go (x:xs) = case snd x of
      VMerge -> getSeg (belowS . fst $ x) (fst x) : go xs
      VSplit -> getSeg (aboveS . fst $ x) (fst x) : go xs
      _      -> [] ++ go xs
    go [] = []
    getSeg :: [PT] -- all points above/below the current point
           -> PT   -- current point
           -> Maybe Segment
    getSeg [] _ = Nothing
    getSeg (z:zs) pt
      | isInsidePoly pts (z, pt) = Just (z, pt)
      | otherwise = getSeg zs pt
    aboveS :: PT -> [PT]
    aboveS pt = tail . dropWhile (/= pt) $ sortedYX pts
    belowS :: PT -> [PT]
    belowS pt = reverse . takeWhile (/= pt) $ sortedYX pts


-- |Triangulate a y-monotone polygon.
triangulate :: [PT] -> [[PT]]
triangulate pts =
  go pts . A.first reverse . splitAt 3 . reverse . sortedYX $ pts
  where
    go :: [PT]          -- current polygon
       -> ([PT], [PT])  -- (stack of visited vertices, rest)
                        --  sorted by Y-coordinate
       -> [[PT]]
    go xs (p@[_, _], r:rs) = go xs (r:p, rs)
    go xs (p@(u:vi:vi1:ys), rs)
      -- case 1 and 3
      | adjacent u (last p) xs =
          splitPoly xs (u, (last . init) p)
          ++ go (fromMaybe []
                  . headMay
                  . nonTriangleOnly
                  . splitPoly xs
                  $ (u, (last . init) p))
                (init p, rs)
      -- case 2
      | adjacent u vi xs && (not . null) rs =
          if getAngle (vp2 vi u) (vp2 vi vi1) < pi / 2
          then splitPoly xs (u, vi1)
                ++ go (fromMaybe []
                        . headMay
                        . nonTriangleOnly
                        . splitPoly xs
                        $ (u, vi1))
                      (u:vi1:ys, rs)
          else go xs (head rs:p, tail rs)
      | otherwise = [[]]
    go _ _ = [[]]
