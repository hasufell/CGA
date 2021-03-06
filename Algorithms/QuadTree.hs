module Algorithms.QuadTree
    (quadTree,
    quadTreeSquares,
    qtFoldl,
    qtFoldr,
    goQuad,
    findNeighbor,
    lookupByPath',
    getSquareByZipper,
    rootNode,
    quadTreeToRoseTree,
    lookupByNeighbors,
    Orient(North,East,West,South),
    Quad(NW,NE,SW,SE),
    QuadTree,
    QTZipper)
  where

import Algebra.Vector
import Data.Foldable (foldlM)
import Data.List (partition)
import Data.Maybe (fromJust)
import Data.Tree
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
data QTCrumb a = NWCrumb (QuadTree a) (QuadTree a) (QuadTree a)
             | NECrumb (QuadTree a) (QuadTree a) (QuadTree a)
             | SWCrumb (QuadTree a) (QuadTree a) (QuadTree a)
             | SECrumb (QuadTree a) (QuadTree a) (QuadTree a)
  deriving (Show, Eq)

-- |Zipper for the QuadTree.
type QTZipper a = (QuadTree a, [QTCrumb a])

-- |Orientation.
data Orient = North | South | East | West
  deriving (Show)


-- |Get a sub-square of the current square, e.g. nw, ne, sw or se.
nwSq, neSq, swSq, seSq :: ((Double, Double), (Double, Double)) -- ^ current square
                       -> ((Double, Double), (Double, Double)) -- ^ sub-square
nwSq ((xl, yl), (xu, yu)) = (,)  (xl, (yl + yu) / 2)  ((xl + xu) / 2, yu)
neSq ((xl, yl), (xu, yu)) = (,)  ((xl + xu) / 2, (yl + yu) / 2)  (xu, yu)
swSq ((xl, yl), (xu, yu)) = (,)  (xl, yl)  ((xl + xu) / 2, (yl + yu) / 2)
seSq ((xl, yl), (xu, yu)) = (,)  ((xl + xu) / 2, yl)  (xu, (yl + yu) / 2)


-- |Check whether the current Node is an nw, ne, sw or se child of it's
-- parent.
isNWchild, isNEchild, isSWchild, isSEchild :: QTZipper a -> Bool
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
quadTree :: [P2 Double]                                 -- ^ the points to divide
         -> ((Double, Double), (Double, Double)) -- ^ the initial square around the points
         -> QuadTree (P2 Double)                          -- ^ the quad tree
quadTree []   _ = TNil
quadTree [pt] _ = TLeaf pt
quadTree pts sq = TNode (quadTree nWPT . nwSq $ sq) (quadTree nEPT . neSq $ sq)
                        (quadTree sWPT . swSq $ sq) (quadTree sEPT . seSq $ sq)
  where
    -- this sets the priority in case a point is between multiple quads
    (sWPT, sWO) = flip partition pts . inRange . swSq $ sq
    (nWPT, nWO) = flip partition sWO . inRange . nwSq $ sq
    (nEPT, nEO) = flip partition nWO . inRange . neSq $ sq
    sEPT        = flip filter    nEO . inRange . seSq $ sq


-- |Get all squares of a quad tree.
quadTreeSquares :: ((Double, Double), (Double, Double))  -- ^ the initial square around the points
                -> QuadTree (P2 Double)                            -- ^ the quad tree
                -> [((Double, Double), (Double, Double))] -- ^ all squares of the quad tree
quadTreeSquares sq (TNil)              = [sq]
quadTreeSquares sq (TLeaf _)           = [sq]
quadTreeSquares sq (TNode nw ne sw se) =
  quadTreeSquares (nwSq sq) nw ++ quadTreeSquares (neSq sq) ne ++
  quadTreeSquares (swSq sq) sw ++ quadTreeSquares (seSq sq) se


-- |Get the current square of the zipper, relative to the given top
-- square.
getSquareByZipper :: ((Double, Double), (Double, Double)) -- ^ top square
                  -> QTZipper a
                  -> ((Double, Double), (Double, Double)) -- ^ current square
getSquareByZipper sq z = go sq (reverse . snd $ z)
  where
    go sq' []              = sq'
    go sq' (NWCrumb {}:zs) = go (nwSq sq') zs
    go sq' (NECrumb {}:zs) = go (neSq sq') zs
    go sq' (SWCrumb {}:zs) = go (swSq sq') zs
    go sq' (SECrumb {}:zs) = go (seSq sq') zs


-- |Left fold over the tree.
qtFoldl :: (a -> QuadTree b -> a) -> a -> QuadTree b -> a
qtFoldl f sv qt@(TNode nw ne sw se) = foldl (qtFoldl f)
                                            (f sv qt)
                                            [nw, ne, sw, se]
qtFoldl f sv qt = f sv qt


-- |Right fold over the tree.
qtFoldr :: (QuadTree b -> a -> a) -> a -> QuadTree b -> a
qtFoldr f sv qt = qtFoldl (\g b x -> g (f b x)) id qt sv


-- |Go to nw, ne, sw or se from the current node, one level deeper.
goNW, goNE, goSW, goSE :: QTZipper a -> Maybe (QTZipper a)
goNW (TNode nw ne sw se, bs) = Just (nw, NWCrumb ne sw se:bs)
goNW _                       = Nothing
goNE (TNode nw ne sw se, bs) = Just (ne, NECrumb nw sw se:bs)
goNE _                       = Nothing
goSW (TNode nw ne sw se, bs) = Just (sw, SWCrumb nw ne se:bs)
goSW _                       = Nothing
goSE (TNode nw ne sw se, bs) = Just (se, SECrumb nw ne sw:bs)
goSE _                       = Nothing


-- |Go to the given Quad from the current Node, one level deeper.
goQuad :: Quad -> QTZipper a -> Maybe (QTZipper a)
goQuad q = case q of
  NW -> goNW
  NE -> goNE
  SW -> goSW
  SE -> goSE


-- |Go up to the parent node, if any.
goUp :: QTZipper a -> Maybe (QTZipper a)
goUp (qt, NWCrumb ne sw se:bs) = Just (TNode qt ne sw se, bs)
goUp (qt, NECrumb nw sw se:bs) = Just (TNode nw qt sw se, bs)
goUp (qt, SWCrumb nw ne se:bs) = Just (TNode nw ne qt se, bs)
goUp (qt, SECrumb nw ne sw:bs) = Just (TNode nw ne sw qt, bs)
goUp _                         = Nothing


-- |Get the root node.
rootNode :: QTZipper a -> QTZipper a
rootNode (qt, []) = (qt, [])
rootNode z        = rootNode . fromJust . goUp $ z


-- |Look up a node by a given path of Quads.
lookupByPath' :: [Quad] -> QuadTree a -> Maybe (QTZipper a)
lookupByPath' qs qt = foldlM (flip goQuad) (qt, []) qs


-- |Find the north, south, east or west neighbor of a given node.
findNeighbor :: Orient -> QTZipper a -> Maybe (QTZipper a)
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


lookupByNeighbors :: [Orient] -> QTZipper a -> Maybe (QTZipper a)
lookupByNeighbors = flip (foldlM (flip findNeighbor))


quadTreeToRoseTree :: QTZipper (P2 Double) -> Tree String
quadTreeToRoseTree z' = go (rootNode z')
  where
    go z = case z of
      (TNil, _)    -> Node markAndPrintOrigin []
      (TLeaf a, _) -> Node (markAndPrintOrigin ++ "\n" ++ (show . unp2 $ a)) []
      _            -> Node markAndPrintOrigin
                        [go (fromJust . goNW $ z)
                        , go (fromJust . goNE $ z)
                        , go (fromJust . goSW $ z)
                        , go (fromJust . goSE $ z)]
      where
        markAndPrintOrigin
          -- HACK: in order to give specific nodes a specific color
          | z' == z   = "* " ++ printOrigin
          | otherwise = printOrigin
        printOrigin
          | isNWchild z = "NW"
          | isNEchild z = "NE"
          | isSWchild z = "SW"
          | isSEchild z = "SE"
          | otherwise   = "root"
