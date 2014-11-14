module Algorithms.RangeSearch.Core
    (quadTree,
    quadTreeSquares,
    qtFoldl,
    qtFoldr,
    goQuad,
    findNeighbor,
    lookupByPath',
    getSquareByZipper,
    rootNode,
    testArr,
    Orient(North,East,West,South),
    Quad(NW,NE,SW,SE),
    QuadTree,
    Zipper)
  where

import Algebra.VectorTypes
import Algebra.Vector
import Data.Foldable (foldlM)
import Data.Maybe (fromJust)
import Diagrams.TwoD.Types


-- |The quad tree structure.
data QuadTree a
  -- |An empty node.
  = TNil
  -- |A leaf containing some value.
  | TLeaf a
  -- |A node with four children.
  | TNode (QuadTree a) (QuadTree a)   -- NW NE
          (QuadTree a) (QuadTree a)   -- SW SE
  deriving (Show, Eq)

-- |Represents a Quadrant in the 2D plane.
data Quad = NW | NE
          | SW | SE
  deriving (Show)

-- |A Crumb used for the QuadTree Zipper.
data Crumb a = NWCrumb (QuadTree a) (QuadTree a) (QuadTree a)
             | NECrumb (QuadTree a) (QuadTree a) (QuadTree a)
             | SWCrumb (QuadTree a) (QuadTree a) (QuadTree a)
             | SECrumb (QuadTree a) (QuadTree a) (QuadTree a)
  deriving (Show, Eq)

-- |A list of Crumbs.
type Breadbrumbs a = [Crumb a]

-- |Zipper for the QuadTree.
type Zipper a = (QuadTree a, Breadbrumbs a)

-- |Orientation.
data Orient = North | South | East | West
  deriving (Show)


-- |Get a sub-square of the current square, e.g. nw, ne, sw or se.
nwSq, neSq, swSq, seSq :: Square -> Square
nwSq ((xl, xu), (yl, yu)) = (,)  (xl, (xl + xu) / 2)  ((yl + yu) / 2, yu)
neSq ((xl, xu), (yl, yu)) = (,)  ((xl + xu) / 2, xu)  ((yl + yu) / 2, yu)
swSq ((xl, xu), (yl, yu)) = (,)  (xl, (xl + xu) / 2)  (yl, (yl + yu) / 2)
seSq ((xl, xu), (yl, yu)) = (,)  ((xl + xu) / 2, xu)  (yl, (yl + yu) / 2)


-- |Check whether the current Node is an nw, ne, sw or se child of it's
-- parent.
isNWchild, isNEchild, isSWchild, isSEchild :: Zipper a -> Bool
isNWchild (_, NWCrumb {}:_) = True
isNWchild _                 = False
isNEchild (_, NECrumb {}:_) = True
isNEchild _                 = False
isSWchild (_, SWCrumb {}:_) = True
isSWchild _                 = False
isSEchild (_, SECrumb {}:_) = True
isSEchild _                 = False


-- |Builds a quadtree of a list of points which recursively divides up 2D
-- space into quadrants, so that every leaf-quadrant stores either zero or one
-- point.
quadTree :: [PT]        -- ^ the points to divide
         -> Square      -- ^ the initial square around the points
         -> QuadTree PT -- ^ the quad tree
quadTree pts' sq' = go (flip filter pts' . inRange $ sq') sq'
  where
    go []   _ = TNil
    go [pt] _ = TLeaf pt
    go pts sq = TNode (quadTree pts . nwSq $ sq) (quadTree pts . neSq $ sq)
                      (quadTree pts . swSq $ sq) (quadTree pts . seSq $ sq)


-- |Get all squares of a quad tree.
quadTreeSquares :: Square      -- ^ the initial square around the points
                -> QuadTree PT -- ^ the quad tree
                -> [Square]    -- ^ all squares of the quad tree
quadTreeSquares sq (TNil)              = [sq]
quadTreeSquares sq (TLeaf _)           = [sq]
quadTreeSquares sq (TNode nw ne sw se) =
  quadTreeSquares (nwSq sq) nw ++ quadTreeSquares (neSq sq) ne ++
  quadTreeSquares (swSq sq) sw ++ quadTreeSquares (seSq sq) se


-- |Get the current square of the zipper, relative to the to the given top
-- square.
getSquareByZipper :: Square -> Zipper a -> Square
getSquareByZipper sq z = go sq (reverse . snd $ z)
  where
    go sq' []              = sq'
    go sq' (NWCrumb {}:zs) = go (nwSq sq') zs
    go sq' (NECrumb {}:zs) = go (neSq sq') zs
    go sq' (SWCrumb {}:zs) = go (swSq sq') zs
    go sq' (SECrumb {}:zs) = go (seSq sq') zs


-- |Left fold over the tree leafs.
qtFoldl :: (a -> b -> a) -> a -> QuadTree b -> a
qtFoldl _ sv (TNil)              = sv
qtFoldl f sv (TLeaf a)           = f sv a
qtFoldl f sv (TNode nw ne sw se) = foldl (qtFoldl f) sv [nw, ne, sw, se]


-- |Right fold over the tree leafs.
qtFoldr :: (b -> a -> a) -> a -> QuadTree b -> a
qtFoldr f sv qt = qtFoldl (\g b x -> g (f b x)) id qt sv


-- |Go to nw, ne, sw or se from the current node, one level deeper.
goNW, goNE, goSW, goSE :: Zipper a -> Maybe (Zipper a)
goNW (TNode nw ne sw se, bs) = Just (nw, NWCrumb ne sw se:bs)
goNW _                       = Nothing
goNE (TNode nw ne sw se, bs) = Just (ne, NECrumb nw sw se:bs)
goNE _                       = Nothing
goSW (TNode nw ne sw se, bs) = Just (sw, SWCrumb nw ne se:bs)
goSW _                       = Nothing
goSE (TNode nw ne sw se, bs) = Just (se, SECrumb nw ne sw:bs)
goSE _                       = Nothing


-- |Go to the given Quad from the current Node, one level deeper.
goQuad :: Quad -> Zipper a -> Maybe (Zipper a)
goQuad q = case q of
  NW -> goNW
  NE -> goNE
  SW -> goSW
  SE -> goSE


-- |Go up to the parent node, if any.
goUp :: Zipper a -> Maybe (Zipper a)
goUp (qt, NWCrumb ne sw se:bs) = Just (TNode qt ne sw se, bs)
goUp (qt, NECrumb nw sw se:bs) = Just (TNode nw qt sw se, bs)
goUp (qt, SWCrumb nw ne se:bs) = Just (TNode nw ne qt se, bs)
goUp (qt, SECrumb nw ne sw:bs) = Just (TNode nw ne sw qt, bs)
goUp _                         = Nothing


-- |Get the root node.
rootNode :: Zipper a -> Zipper a
rootNode (qt, []) = (qt, [])
rootNode z        = rootNode . fromJust . goUp $ z


-- |Look up a node by a given path of Quads.
lookupByPath' :: [Quad] -> QuadTree a -> Maybe (Zipper a)
lookupByPath' qs qt = foldlM (flip goQuad) (qt, []) qs


-- |Find the north, south, east or west neighbor of a given node.
findNeighbor :: Orient -> Zipper a -> Maybe (Zipper a)
findNeighbor ot zr = case ot of
  North -> go isSWchild isSEchild isNWchild goNW goNE goSW goSE zr
  South -> go isNWchild isNEchild isSWchild goSW goSE goNW goNE zr
  East  -> go isNWchild isSWchild isNEchild goNE goSE goNW goSW zr
  West  -> go isNEchild isSEchild isNWchild goNW goSW goNE goSE zr
  where
    go _ _ _ _ _ _ _  (_, []) = Nothing
    go is1 is2 is3 go1 go2 go3 go4 z@(_, _:_)
      | is1 z = goUp z >>= go1
      | is2 z = goUp z >>= go2
      | otherwise = checkParent
                      . go is1 is2 is3 go1 go2 go3 go4
                      . fromJust
                      . goUp
                      $ z
      where
        checkParent (Just (z'@(TNode {}, _)))
          | is3 z     = go3 z'
          | otherwise = go4 z'
        checkParent (Just z') = Just z'
        checkParent _         = Nothing



testArr :: [PT]
testArr = [p2 (200.0, 450.0),
           p2 (400.0, 350.0),
           p2 (100.0, 300.0),
           p2 (25.0 , 350.0),
           p2 (225.0, 225.0),
           p2 (400.0, 150.0),
           p2 (300.0, 100.0),
           p2 (300.0, 300.0),
           p2 (300.0, 350.0),
           p2 (50.0 , 450.0),
           p2 (100.0, 25.0)]
