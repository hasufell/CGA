{-# OPTIONS_HADDOCK ignore-exports #-}
{-# LANGUAGE ViewPatterns #-}

module Algorithms.PolygonTriangulation where

import Algebra.Polygon
import Algebra.Vector
import qualified Control.Arrow as A
import Data.List
import Data.Maybe
import Safe
import Diagrams.Coordinates
import MyPrelude


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
  (ptCmpY next v == LT) && (ptCmpY prev v == LT) && (cw next v prev)


-- |Whether the vertex, given it's next and previous vertex,
-- is a split vertex.
isVSplit :: PT  -- ^ previous vertex
         -> PT  -- ^ vertice to check
         -> PT  -- ^ next vertex
         -> Bool
isVSplit prev v next =
  (ptCmpY prev v == LT) && (ptCmpY next v == LT) && (cw prev v next)


-- |Whether the vertex, given it's next and previous vertex,
-- is an end vertex.
isVEnd :: PT  -- ^ previous vertex
       -> PT  -- ^ vertice to check
       -> PT  -- ^ next vertex
       -> Bool
isVEnd prev v next =
  (ptCmpY prev v == GT) && (ptCmpY next v == GT) && (cw next v prev)


-- |Whether the vertex, given it's next and previous vertex,
-- is a merge vertex.
isVMerge :: PT  -- ^ previous vertex
         -> PT  -- ^ vertice to check
         -> PT  -- ^ next vertex
         -> Bool
isVMerge prev v next =
  (ptCmpY next v == GT) && (ptCmpY prev v == GT) && (cw prev v next)


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



-- |Check if polygon is y-monotone.
isYmonotone :: [PT] -> Bool
isYmonotone poly =
  not
  . any (\x -> x == VSplit || x == VMerge)
  . fmap snd
  $ classifyList poly


monotonize :: [PT] -> [[PT]]
monotonize pts
  | isYmonotone pts = partitionPoly pts
  | and . fmap isYmonotone $ maybeMonotone =
      concat . fmap partitionPoly $ maybeMonotone
  | otherwise = (\(x, y) -> x ++ (concat . fmap monotonize $ y))
                  (partition isYmonotone maybeMonotone)
  where
    go (x:xs) = splitPoly pts x ++ go xs
    go _      = []
    maybeMonotone = go (monotoneDiagonals pts)



monotoneDiagonals :: [PT] -> [(PT, PT)]
monotoneDiagonals pts = catMaybes . go $ classifyList pts
  where
    go (x:xs) = case snd x of
      VMerge -> getSeg (belowS (fst x) pts) (fst x) pts : go xs
      VSplit -> getSeg (aboveS (fst x) pts) (fst x) pts : go xs
      _      -> [] ++ go xs
    go [] = []
    getSeg [] _ _ = Nothing
    getSeg (z:zs) pt pts'
      | isInsidePoly pts (z, pt) = Just (z, pt)
      | otherwise = getSeg zs pt pts'
    aboveS pt pts' = tail . dropWhile (/= pt) $ sortedYX pts'
    belowS pt pts' = reverse . takeWhile (/= pt) $ sortedYX pts'


partitionPoly :: [PT] -> [[PT]]
partitionPoly pts =
  go pts . A.first reverse . splitAt 3 . reverse . sortedYX $ pts
  where
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
