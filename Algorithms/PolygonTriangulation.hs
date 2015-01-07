{-# OPTIONS_HADDOCK ignore-exports #-}
{-# LANGUAGE ViewPatterns #-}

module Algorithms.PolygonTriangulation where

import Algebra.Vector
import Diagrams.Coordinates


data VCategory = VStart
               | VEnd
               | VRegular
               | VSplit
               | VMerge
  deriving (Show)


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


-- A point u is below of v ( u < v ),
-- if u_y < v_y or u_y = v_y and u_x > v_x.
below :: PT  -- ^ is this one below the other?
      -> PT
      -> Bool
below (coords -> ux :& uy) (coords -> vx :& vy) =
  (uy <= vy ) && (ux > vx)


-- A point u is above of v , if v < u.
above :: PT  -- ^ is this one above the other?
      -> PT
      -> Bool
above = flip below
