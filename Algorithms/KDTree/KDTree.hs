{-# OPTIONS_HADDOCK ignore-exports #-}

module Algorithms.KDTree.KDTree (kdTree
                                 , kdFoldl
                                 , kdFoldr
                                 , kdTreeToRoseTree
                                 , rangeSearch
                                 , getValS
                                 , isLeaf
                                 , getVal
                                 , getDirection
                                 , goLeft
                                 , goRight
                                 , Direction(Vertical, Horizontal)
                                 , KDTree(KTNil, KTNode))
  where


import Algebra.VectorTypes
import Algebra.Vector
import Data.Maybe (fromJust, catMaybes)
import Data.Tree
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


-- |Execute a range search in O(log n). It returns a tuple
-- of the points found in the range and also gives back a pretty
-- rose tree suitable for printing.
rangeSearch :: KDTree PT -> Square -> ([PT], Tree String)
rangeSearch kd' sq' = (goPt kd' sq', goTree kd' sq' True)
  where
    -- either y1 or x1 depending on the orientation
    p1'  dir ((x1, _), (y1, _)) = if' (dir == Vertical) y1 x1
    -- either y2 or x2 depending on the orientation
    p2'  dir ((_, x2), (_, y2)) = if' (dir == Vertical) y2 x2
    -- either the second or first of the tuple, depending on the orientation
    cur' dir = if' (dir == Vertical) snd fst
    -- All points in the range.
    goPt :: KDTree PT -> Square -> [PT]
    goPt KTNil _ = []
    goPt (KTNode ln pt dir rn) sq =
      [pt | inRange sq pt]
      ++ (if' (p1' dir sq < (cur' dir . unp2 $ pt))
              (goPt ln sq)
              []
         )
      ++ (if' ((cur' dir . unp2 $ pt) < p2' dir sq)
              (goPt rn sq)
              [])
        where
    -- A pretty rose tree suitable for printing.
    goTree :: KDTree PT -> Square -> Bool -> Tree String
    goTree KTNil _ _ = Node "nil" []
    goTree (KTNode ln pt dir rn) sq vis =
      Node treeText
           [if' (p1' dir sq < (cur' dir . unp2 $ pt))
                (goTree ln sq vis)
                (goTree ln sq False)
           , if' ((cur' dir . unp2 $ pt) < p2' dir sq)
                 (goTree rn sq vis)
                 (goTree rn sq False)]
        where
          treeText
            | vis && inRange sq pt = "** " ++ (show . unp2 $ pt)
            | vis                  = "* " ++ (show . unp2 $ pt)
            | otherwise            = show . unp2 $ pt



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


-- |Convert a kd-tree to a rose tree, for pretty printing.
kdTreeToRoseTree :: KDTree PT -> Tree String
kdTreeToRoseTree (KTNil) = Node "nil" []
kdTreeToRoseTree (KTNode ln val _ rn) =
  Node (show . unp2 $ val) [kdTreeToRoseTree ln, kdTreeToRoseTree rn]


goLeft :: KDTree a -> Maybe (KDTree a)
goLeft (KTNode ln _ _ _) = Just ln
goLeft _                 = Nothing


goRight :: KDTree a -> Maybe (KDTree a)
goRight (KTNode _ _ _ rn) = Just rn
goRight _                 = Nothing

