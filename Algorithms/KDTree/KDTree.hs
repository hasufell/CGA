module Algorithms.KDTree.KDTree where


import Algebra.VectorTypes
import Algebra.Vector
import Data.Maybe (fromJust, catMaybes)
import Diagrams.TwoD.Types
import MyPrelude (pivot,if',Not, not')
import Safe


-- |The KDTree data structure.
data KDTree a
  -- |An empty node.
  = KTNil
  -- |A node with a value and a left and right child
  | KTNode (KDTree a) a Direction (KDTree a)
  deriving (Show, Eq)

data Direction = Vertical
               | Horizontal
  deriving (Show, Eq, Enum)

instance Not Direction where
  not' Vertical   = Horizontal
  not' Horizontal = Vertical


-- |Construct a kd-tree from a list of points in O(n log n).
kdTree :: [PT]      -- ^ list of points to construct the kd-tree from
       -> Direction -- ^ initial direction of the root-node
       -> KDTree PT -- ^ resulting kd-tree
kdTree xs' = go (sortedX xs') (sortedY xs')
  where
    go [] _ _ = KTNil
    go _ [] _ = KTNil
    go xs ys dir =
      KTNode (go x1 y1 (not' dir))
             (fromJust . pivot $ if' (dir == Vertical) ys xs)
             dir
             (go x2 y2 (not' dir))
      where
        ((x1, x2), (y1, y2)) = if' (dir == Vertical)
                                   (partitionY (xs, ys))
                                   (partitionX (xs, ys))


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


-- |Partition two sorted lists of points X and Y against the pivot of
-- Y. This function is unsafe as it does not check if there is a valid
-- pivot.
partitionY :: ([PT], [PT])                 -- ^ both lists (X, Y)
           -> (([PT], [PT]), ([PT], [PT])) -- ^ ((x1, x2), (y1, y2))
partitionY (xs, ys) = partition' (fromJust . pivot $ ys) (xs, ys)


-- |Partition two sorted lists of points X and Y against the pivot of
-- X. This function is unsafe as it does not check if there is a valid
-- pivot.
partitionX :: ([PT], [PT])                 -- ^ both lists (X, Y)
           -> (([PT], [PT]), ([PT], [PT])) -- ^ ((x1, x2), (y1, y2))
partitionX (xs, ys) = (\(x, y) -> (y, x))
                      . partition' (fromJust . pivot $ xs) $ (ys, xs)


-- |Execute a range search in O(log n).
rangeSearch :: KDTree PT -> Square -> [PT]
rangeSearch KTNil _ = []
rangeSearch (KTNode ln pt Vertical rn) sq@(_, (y1, y2)) =
    [pt | inRange sq pt]
    ++ (if y1 < (snd . unp2 $ pt) then rangeSearch ln sq else [])
    ++ (if (snd . unp2 $ pt) < y2 then rangeSearch rn sq else [])
rangeSearch (KTNode ln pt Horizontal rn) sq@((x1, x2), _) =
    [pt | inRange sq pt]
    ++ (if x1 < (fst . unp2 $ pt) then rangeSearch ln sq else [])
    ++ (if (fst . unp2 $ pt) < x2 then rangeSearch rn sq else [])



-- |Left fold over ALL tree nodes.
kdFoldl :: (a -> KDTree b -> a) -> a -> KDTree b -> a
kdFoldl f sv kd@(KTNode ln _ _ rn) = foldl (kdFoldl f) (f sv kd) [ln, rn]
kdFoldl f sv kd = f sv kd


-- |Right fold over ALL tree nodes.
kdFoldr :: (KDTree b -> a -> a) -> a -> KDTree b -> a
kdFoldr f sv kd = kdFoldl (\g b x -> g (f b x)) id kd sv


-- |Get all values of a tree.
getValS :: KDTree a -> [a]
getValS = catMaybes . kdFoldl (\x y -> x ++ [getVal y]) []


-- |Whether the tree is a leaf.
isLeaf :: KDTree a -> Bool
isLeaf (KTNode KTNil _ _ KTNil) = True
isLeaf _                      = False


-- |Get the value of the root node of the tree. Returns Nothing if it's a
-- leaf.
getVal :: KDTree a -> Maybe a
getVal (KTNode _ val _ _) = Just val
getVal _                = Nothing


-- |Get the direction of the current node/level.
getDirection :: KDTree a -> Maybe Direction
getDirection (KTNode _ _ dir _) = Just dir
getDirection _                  = Nothing


