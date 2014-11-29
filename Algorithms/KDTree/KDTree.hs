module Algorithms.KDTree.KDTree where


import Algebra.VectorTypes
import Algebra.Vector
import Data.Maybe (fromJust, catMaybes)
import Diagrams.TwoD.Types
import MyPrelude (pivot)
import Safe


-- |The KDTree data structure.
data KDTree a
  -- |An empty node.
  = KTNil
  -- |A node with a value and a left and right child
  | KTNode (KDTree a) a (KDTree a)
  deriving (Show, Eq)

data Crumb a = Left (KDTree a)
             | Right (KDTree a)
  deriving (Show, Eq)

-- |A list of Crumbs.
type Breadcrumbs a = [Crumb a]

-- |Zipper for the KDTree.
type Zipper a = (KDTree a, Breadcrumbs a)

data Direction = Vertical
               | Horizontal


-- |Construct a kd-tree from a list of points in O(n log n).
kdTree :: [PT]
       -> KDTree PT
kdTree xs' = go (sortedX xs') (sortedY xs') Horizontal
  where
    go [] _ _ = KTNil
    go _ [] _ = KTNil
    go xs ys Vertical =
      KTNode (go x1 y1 Horizontal)
             (fromJust . pivot $ ys)
             (go x2 y2 Horizontal)
      where
        ((x1, x2), (y1, y2)) = partition' (fromJust . pivot $ ys) (xs, ys)
    go xs ys Horizontal =
      KTNode (go x1 y1 Vertical)
             (fromJust . pivot $ xs)
             (go x2 y2 Vertical)
      where
        ((y1, y2), (x1, x2)) = partition' (fromJust . pivot $ xs) (ys, xs)


-- |Partitions two sorted list of points X and Y against a pivot.
-- If you want to partition against the pivot of Y, then you pass
--   partition' (pivot ys) (xs, ys)
-- and get ((x1, x2), (y1, y2)).
-- If you want to partition against the pivot of X, then you pass
--   partition' (pivot xs) (ys, xs)
-- and get ((y1, y2), (x1, x2)).
partition' :: PT                           -- ^ the pivot to partition against
           -> ([PT], [PT])                 -- ^ both lists (X, Y) or (Y, X)
           -> (([PT], [PT]), ([PT], [PT])) -- ^ ((x1, x2), (y1, y2)) or
                                           --   ((y1, y2), (x1, x2))
partition' piv (xs, ys) = ((x1, x2), (y1, y2))
  where
    y1 = takeWhile (/= piv) ys
    y2 = tailDef [] . dropWhile (/= piv) $ ys
    x1 = foldr (\x y -> [x | x `elem` y1] ++ y) [] xs
    x2 = foldr (\x y -> [x | x `elem` y2] ++ y) [] xs


-- |Execute a range search in O(log n).
rangeSearch :: KDTree PT -> Square -> [PT]
rangeSearch = go Horizontal
  where
    go _ KTNil _ = []
    go Vertical (KTNode ln pt rn) sq@(_, (y1, y2)) =
      [pt | inRange sq pt]
      ++ (if y1 < (snd . unp2 $ pt) then go Horizontal ln sq else [])
      ++ (if (snd . unp2 $ pt) < y2 then go Horizontal rn sq else [])
    go Horizontal (KTNode ln pt rn) sq@((x1, x2), _) =
      [pt | inRange sq pt]
      ++ (if x1 < (fst . unp2 $ pt) then go Vertical ln sq else [])
      ++ (if (fst . unp2 $ pt) < x2 then go Vertical rn sq else [])


-- |Left fold over ALL tree nodes.
kdFoldl :: (a -> KDTree b -> a) -> a -> KDTree b -> a
kdFoldl f sv kd@(KTNode ln _ rn) = foldl (kdFoldl f) (f sv kd) [ln, rn]
kdFoldl f sv kd = f sv kd


-- |Right fold over ALL tree nodes.
kdFoldr :: (KDTree b -> a -> a) -> a -> KDTree b -> a
kdFoldr f sv kd = kdFoldl (\g b x -> g (f b x)) id kd sv


-- |Get all values of a tree.
getValS :: KDTree a -> [a]
getValS = catMaybes . kdFoldl (\x y -> x ++ [getVal y]) []


-- |Whether the tree is a leaf.
isLeaf :: KDTree a -> Bool
isLeaf (KTNode KTNil _ KTNil) = True
isLeaf _                      = False


-- |Get the value of the root node of the tree. Returns Nothing if it's a
-- leaf.
getVal :: KDTree a -> Maybe a
getVal (KTNode _ val _) = Just val
getVal _                = Nothing

