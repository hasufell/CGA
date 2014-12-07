{-# OPTIONS_HADDOCK ignore-exports #-}

module Graphics.Diagram.AlgoDiags where

import Algebra.Vector(PT,Square)
import Algorithms.GrahamScan
import Algorithms.QuadTree
import Algorithms.KDTree
import Algorithms.PolygonIntersection
import Data.Maybe
import Data.Monoid
import Data.Tree
import Diagrams.Backend.Cairo
import Diagrams.Prelude hiding ((<>))
import Diagrams.TwoD.Layout.Tree
import Graphics.Diagram.Core
import Parser.PathParser


-- |Draw the lines of the polygon.
polyLines :: Diag
polyLines = Diag f
  where
    f _ [] = mempty
    f _ (x:y:_) =
      strokePoly x <> strokePoly y
      where
        strokePoly x' = (strokeTrail . fromVertices $ x' ++ [head x'])
          # moveTo (head x') # lc black
    f _ _  = mempty


-- |Show the intersection points of two polygons as red dots.
polyIntersection :: Diag
polyIntersection = Diag f
  where
    f p (x:y:_) = drawP vtpi (dotSize p) # fc red # lc red
      where
        vtpi = intersectionPoints . sortLexPolys $ (sortLexPoly x, sortLexPoly y)
    f _ _ = mempty


-- |Show the coordinate text of the intersection points of two polygons.
polyIntersectionText :: Diag
polyIntersectionText = Diag f
  where
    f p (x:y:_)
      | showCoordText p = position . zip vtpi $ (pointToTextCoord # fc red <$> vtpi)
          # translate (r2 (0, 10))
      | otherwise = mempty
      where
        vtpi = intersectionPoints
                . sortLexPolys
                $ (sortLexPoly x,
                   sortLexPoly y)
    f _ _ = mempty


-- |Create a diagram which shows the points of the convex hull.
convexHP :: Diag
convexHP = Diag f
  where
    f p [vt] = drawP (grahamCH vt) (dotSize p) # fc red # lc red
    f _ _ = mempty


-- |Show coordinates as text above the convex hull points.
convexHPText :: Diag
convexHPText = Diag f
  where
    f p [vt]
      | showCoordText p =
          position $ zip vtchf (pointToTextCoord <$> vtchf) # translate (r2 (0, 10))
      | otherwise = mempty
      where
        vtchf = grahamCH vt
    f _ _ = mempty


-- |Create a diagram which shows the lines along the convex hull
-- points.
convexHLs :: Diag
convexHLs = Diag f
  where
    f _ [vt] =
      (strokeTrail . fromVertices . flip (++) [head $ grahamCH vt] . grahamCH $ vt)
        # moveTo (head $ grahamCH vt) # lc red
    f _ _ = mempty


-- |Create list of diagrama which describe the lines along points of a half
-- convex hull, for each iteration of the algorithm. Which half is chosen
-- depends on the input.
convexHStepsLs :: Diag
convexHStepsLs = GifDiag f
  where
    f _ col g vt = fmap mkChDiag (g vt)
      where
        mkChDiag vt' = (strokeTrail . fromVertices $ vt') # moveTo (head vt') # lc col


-- |Create a diagram that shows all squares of the RangeSearch algorithm
-- from the quad tree.
squares :: Diag
squares = Diag f
  where
    f p [vt] =
      mconcat
      $ (uncurry rectByDiagonal # lw ultraThin)
        <$>
        (quadTreeSquares (xDimension p, yDimension p)
          . quadTree vt
          $ (xDimension p, yDimension p))
    f _ _ = mempty


-- |Draw the squares of the kd-tree.
kdSquares :: Diag
kdSquares = Diag f
  where
    f p [vt] =
      mconcat
      . fmap (uncurry (~~))
      $ kdLines (kdTree vt Horizontal) (xDimension p, yDimension p)
      where
        -- Gets all lines that make up the kdSquares. Every line is
        -- described by two points, start and end respectively.
        kdLines :: KDTree PT -> Square -> [(PT, PT)]
        kdLines (KTNode ln pt Horizontal rn) ((xmin, xmax), (ymin, ymax)) =
          (\(x, _) -> [(p2 (x, ymin), p2 (x, ymax))])
            (unp2 pt)
          ++ kdLines ln ((xmin, x'), (ymin, ymax))
          ++ kdLines rn ((x', xmax), (ymin, ymax))
            where
              (x', _) = unp2 pt
        kdLines (KTNode ln pt Vertical rn) ((xmin, xmax), (ymin, ymax)) =
          (\(_, y) -> [(p2 (xmin, y), p2 (xmax, y))])
            (unp2 pt)
          ++ kdLines ln ((xmin, xmax), (ymin, y'))
          ++ kdLines rn ((xmin, xmax), (y', ymax))
            where
              (_, y') = unp2 pt
        kdLines _ _ = []
    f _ _ = mempty


-- |Draw the range rectangle and highlight the points inside that range.
kdRange :: Diag
kdRange = Diag f
  where
    f p [vt] =
      (uncurry rectByDiagonal # lc red) (rangeSquare p)
        <> drawP ptsInRange (dotSize p) # fc red # lc red
      where
        ptsInRange = fst . rangeSearch (kdTree vt Vertical) $ rangeSquare p
    f _ _ = mempty


-- |The kd-tree visualized as binary tree.
kdTreeDiag :: Diag
kdTreeDiag = Diag f
  where
    f p [vt] =
      -- HACK: in order to give specific nodes a specific color
      renderTree (\n -> case n of
                   '*':'*':_ -> (text n # fontSizeL 5.0)
                                 <> rect 50.0 20.0 # fc green
                   '*':_     -> (text n # fontSizeL 5.0)
                                 <> rect 50.0 20.0 # fc red
                   _         -> (text n # fontSizeL 5.0)
                                 <> rect 50.0 20.0 # fc white)
                 (~~)
                 (symmLayout' (with & slHSep .~ 60 & slVSep .~ 40) roseTree)
      # scale 2 # alignT # bg white
      where
        roseTree = snd
                   . rangeSearch (kdTree vt Vertical)
                   $ rangeSquare p

    f _ _ = mempty


-- |Get the quad tree corresponding to the given points and diagram properties.
qt :: [PT] -> DiagProp -> QuadTree PT
qt vt p = quadTree vt (xDimension p, yDimension p)


-- |Create a diagram that shows a single square of the RangeSearch algorithm
-- from the quad tree in red, according to the given path in quadPath.
quadPathSquare :: Diag
quadPathSquare = Diag f
  where
    f p [vt] =
      (uncurry rectByDiagonal # lw thin # lc red)
      (getSquare (stringToQuads (quadPath p)) (qt vt p, []))
      where
        getSquare :: [Either Quad Orient] -> QTZipper PT -> Square
        getSquare [] z = getSquareByZipper (xDimension p, yDimension p) z
        getSquare (q:qs) z = case q of
          Right x -> getSquare qs (fromMaybe z (findNeighbor x z))
          Left x  -> getSquare qs (fromMaybe z (goQuad x z))
    f _ _  = mempty


-- |Create a list of diagrams that show the walk along the given path
-- through the quad tree.
gifQuadPath :: Diag
gifQuadPath = GifDiag f
  where
    f p col _ vt =
      (uncurry rectByDiagonal # lw thick # lc col)
      <$> getSquares (stringToQuads (quadPath p)) (qt vt p, [])
      where
        getSquares :: [Either Quad Orient] -> QTZipper PT -> [Square]
        getSquares [] z = [getSquareByZipper (xDimension p, yDimension p) z]
        getSquares (q:qs) z = case q of
          Right x -> getSquareByZipper (xDimension p, yDimension p) z :
                       getSquares qs (fromMaybe z (findNeighbor x z))
          Left x  -> getSquareByZipper (xDimension p, yDimension p) z :
                       getSquares qs (fromMaybe z (goQuad x z))


-- |A diagram that shows the full Quad Tree with nodes.
treePretty :: Diag
treePretty = Diag f
  where
    f p [vt] =
      prettyRoseTree (quadTreeToRoseTree
                      . flip getCurQT (qt vt p, [])
                      . stringToQuads
                      . quadPath
                      $ p)
      where
        getCurQT :: [Either Quad Orient] -> QTZipper PT -> QTZipper PT
        getCurQT [] z = z
        getCurQT (q:qs) z = case q of
          Right x -> getCurQT qs (fromMaybe z (findNeighbor x z))
          Left x  -> getCurQT qs (fromMaybe z (goQuad x z))
        prettyRoseTree :: Tree String -> Diagram Cairo R2
        prettyRoseTree tree =
        -- HACK: in order to give specific nodes a specific color
          renderTree (\n -> case head n of
                       '*' -> (text n # fontSizeL 5.0)
                              <> rect 50.0 20.0 # fc red
                       _   -> (text n # fontSizeL 5.0)
                              <> rect 50.0 20.0 # fc white)
                     (~~)
                     (symmLayout' (with & slHSep .~ 60 & slVSep .~ 40) tree)
          # scale 2 # alignT # bg white
    f _ _  = mempty
