{-# OPTIONS_HADDOCK ignore-exports #-}

module Algorithms.PolygonTriangulation where

import Algebra.Polygon
import Algebra.Vector
import qualified Control.Arrow as A
import Data.Maybe
import Diagrams.TwoD.Types
import Safe


data VCategory = VStart
               | VEnd
               | VRegular
               | VSplit
               | VMerge
  deriving (Show, Eq)


-- |Classify all vertices on a polygon into five categories (see VCategory).
classifyList :: [P2 Double] -> [(P2 Double, VCategory)]
classifyList p@(x:y:_:_) =
  -- need to handle the first and last element separately
  [classify (last p) x y] ++ go p ++ [classify (last . init $ p) (last p) x]
  where
    go :: [P2 Double] -> [(P2 Double, VCategory)]
    go (x':y':z':xs) = classify x' y' z' : go (y':z':xs)
    go _          = []
classifyList _ = []


-- |Classify a vertex on a polygon given it's next and previous vertex
-- into five categories (see VCategory).
classify :: P2 Double  -- ^ prev vertex
         -> P2 Double  -- ^ classify this one
         -> P2 Double  -- ^ next vertex
         -> (P2 Double, VCategory)
classify prev v next
  | isVStart prev v next = (v, VStart)
  | isVSplit prev v next = (v, VSplit)
  | isVEnd prev v next   = (v, VEnd)
  | isVMerge prev v next = (v, VMerge)
  | otherwise            = (v, VRegular)


-- |Whether the vertex, given it's next and previous vertex,
-- is a start vertex.
isVStart :: P2 Double  -- ^ previous vertex
         -> P2 Double  -- ^ vertice to check
         -> P2 Double  -- ^ next vertex
         -> Bool
isVStart prev v next =
  ptCmpY next v == LT && ptCmpY prev v == LT && cw next v prev


-- |Whether the vertex, given it's next and previous vertex,
-- is a split vertex.
isVSplit :: P2 Double  -- ^ previous vertex
         -> P2 Double  -- ^ vertice to check
         -> P2 Double  -- ^ next vertex
         -> Bool
isVSplit prev v next =
  ptCmpY prev v == LT && ptCmpY next v == LT && cw prev v next


-- |Whether the vertex, given it's next and previous vertex,
-- is an end vertex.
isVEnd :: P2 Double  -- ^ previous vertex
       -> P2 Double  -- ^ vertice to check
       -> P2 Double  -- ^ next vertex
       -> Bool
isVEnd prev v next =
  ptCmpY prev v == GT && ptCmpY next v == GT && cw next v prev


-- |Whether the vertex, given it's next and previous vertex,
-- is a merge vertex.
isVMerge :: P2 Double  -- ^ previous vertex
         -> P2 Double  -- ^ vertice to check
         -> P2 Double  -- ^ next vertex
         -> Bool
isVMerge prev v next =
  ptCmpY next v == GT && ptCmpY prev v == GT && cw prev v next


-- |Whether the vertex, given it's next and previous vertex,
-- is a regular vertex.
isVRegular :: P2 Double  -- ^ previous vertex
           -> P2 Double  -- ^ vertice to check
           -> P2 Double  -- ^ next vertex
           -> Bool
isVRegular prev v next =
     (not . isVStart prev v $ next)
  && (not . isVSplit prev v $ next)
  && (not . isVEnd prev v $ next)
  && (not . isVMerge prev v $ next)



-- |A polygon P is y-monotone, if it has no split and merge vertices.
isYmonotone :: [P2 Double] -> Bool
isYmonotone poly =
  not
  . any (\x -> x == VSplit || x == VMerge)
  . fmap snd
  $ classifyList poly


-- |Partition P into y-monotone pieces.
monotonePartitioning :: [P2 Double] -> [[P2 Double]]
monotonePartitioning pts
  | isYmonotone pts = [pts]
  | otherwise = go (monotoneDiagonals pts) pts
  where
    go :: [(P2 Double, P2 Double)] -> [P2 Double] -> [[P2 Double]]
    go (x:xs) pts'@(_:_)
      | isYmonotone a && isYmonotone b = [a, b]
      | isYmonotone b = b : go xs a
      | otherwise = a : go xs b
      where
        [a, b] = splitPoly pts' x
    go _ _ = []


-- |Try to eliminate the merge and split vertices by computing the
-- diagonals we have to use for splitting the polygon.
monotoneDiagonals :: [P2 Double] -> [(P2 Double, P2 Double)]
monotoneDiagonals pts = catMaybes . go $ classifyList pts
  where
    go :: [(P2 Double, VCategory)] -> [Maybe (P2 Double, P2 Double)]
    go (x:xs) = case snd x of
      VMerge -> getSeg (belowS . fst $ x) (fst x) : go xs
      VSplit -> getSeg (aboveS . fst $ x) (fst x) : go xs
      _      -> [] ++ go xs
    go [] = []
    getSeg :: [P2 Double] -- all points above/below the current point
           -> P2 Double   -- current point
           -> Maybe (P2 Double, P2 Double)
    getSeg [] _ = Nothing
    getSeg (z:zs) pt
      | isInsidePoly pts (z, pt) = Just (z, pt)
      | otherwise = getSeg zs pt
    aboveS :: P2 Double -> [P2 Double]
    aboveS pt = tail . dropWhile (/= pt) $ sortedYX pts
    belowS :: P2 Double -> [P2 Double]
    belowS pt = reverse . takeWhile (/= pt) $ sortedYX pts


-- |Triangulate a y-monotone polygon.
triangulate :: [P2 Double] -> [[P2 Double]]
triangulate pts =
  go pts . A.first reverse . splitAt 3 . reverse . sortedYX $ pts
  where
    go :: [P2 Double]          -- current polygon
       -> ([P2 Double], [P2 Double])  -- (stack of visited vertices, rest)
                        --  sorted by Y-coordinate
       -> [[P2 Double]]
    go xs (p@[_, _], r:rs) = go xs (r:p, rs)
    go xs (p@(u:vi:vi1:ys), rs)
      -- case 1 and 3
      | adjacent u (last p) xs =
          (triangleOnly . splitPoly xs $ (u, (last . init) p))
          ++ go (fromMaybe []
                  . headMay
                  . nonTriangleOnly
                  . splitPoly xs
                  $ (u, (last . init) p))
                (init p, rs)
      -- case 2
      | adjacent u vi xs && (not . null) rs =
          if getAngle (vp2 vi u) (vp2 vi vi1) < pi / 2
          then (triangleOnly . splitPoly xs $ (u, vi1))
                ++ go (fromMaybe []
                        . headMay
                        . nonTriangleOnly
                        . splitPoly xs
                        $ (u, vi1))
                      (u:vi1:ys, rs)
          else go xs (head rs:p, tail rs)
      | otherwise = []
    go _ _ = []
