{-# OPTIONS_HADDOCK ignore-exports #-}

module Graphics.Diagram.Plotter where

import Algebra.VectorTypes
import Algorithms.ConvexHull.GrahamScan
import Algorithms.QuadTree.QuadTree
import Algorithms.KDTree.KDTree
import Algorithms.PolygonIntersection.Core
import Data.Maybe
import Data.Monoid
import Data.Tree
import Diagrams.Backend.Cairo
import Diagrams.Prelude hiding ((<>))
import Diagrams.TwoD.Layout.Tree
import Graphics.Diagram.Types
import Parser.PathParser


-- |Creates a Diagram that shows the coordinates from the points
-- as dots. The points and thickness of the dots can be controlled
-- via DiagProp.
coordPoints :: Diag
coordPoints = Diag cp
  where
    cp p (Object vt) = drawP vt p
    cp p (Objects vts) = drawP (concat vts) p
    drawP [] _ = mempty
    drawP vt p =
      position (zip vt (repeat dot))
      where
        dot = (circle $ dotSize p :: Diagram Cairo R2) # fc black


-- |Creates a Diagram from a point that shows the coordinates
-- in text format, such as "(1.0, 2.0)".
pointToTextCoord :: PT -> Diagram Cairo R2
pointToTextCoord pt =
  text ("(" ++ (show . trim') x ++ ", " ++ (show . trim') y ++ ")") # scale 10
  where
    trim' :: Double -> Double
    trim' x' = fromInteger . round $ x' * (10^(2 :: Int)) /
                                          (10.0^^(2 :: Int))
    (x, y) = unp2 pt


-- |Show coordinates as text above all points.
coordPointsText :: Diag
coordPointsText = Diag cpt
  where
    cpt p (Object vt)   = drawT vt p
    cpt p (Objects vts) = drawT (concat vts) p
    drawT [] _ = mempty
    drawT vt p
      | showCoordText p =
          position
          $ zip vt (pointToTextCoord <$> vt)
          # translate (r2 (0, 10))
      | otherwise = mempty


-- |Draw the lines of the polygon.
polyLines :: Diag
polyLines = Diag pp
  where
    pp _ (Objects []) = mempty
    pp _ (Objects (x:y:_)) =
      strokePoly x <> strokePoly y
      where
        strokePoly x' =
          (strokeTrail
           . fromVertices
           $ x' ++ [head x'])
          # moveTo (head x') # lc black
    pp _ _  = mempty


-- |Show the intersection points of two polygons as red dots.
polyIntersection :: Diag
polyIntersection = Diag pi'
  where
    pi' p (Objects (x:y:_)) = position (zip vtpi (repeat dot))
      where
        dot  = (circle $ dotSize p :: Diagram Cairo R2) # fc red # lc red
        vtpi = intersectionPoints
               . sortLexPolys
               $ (sortLexPoly x,
                  sortLexPoly y)
    pi' _ _ = mempty


-- |Show the coordinate text of the intersection points of two polygons.
polyIntersectionText :: Diag
polyIntersectionText = Diag pit'
  where
    pit' p (Objects (x:y:_))
      | showCoordText p =
          position
          . zip vtpi
          $ (pointToTextCoord # fc red <$> vtpi)
          # translate (r2 (0, 10))
      | otherwise = mempty
      where
        vtpi = intersectionPoints
                . sortLexPolys
                $ (sortLexPoly x,
                   sortLexPoly y)
    pit' _ _ = mempty


-- |Create a diagram which shows the points of the convex hull.
convexHP :: Diag
convexHP = Diag chp
  where
    chp p (Object vt) =
      position (zip vtch
        (repeat dot))
      where
        dot = (circle $ dotSize p :: Diagram Cairo R2) # fc red # lc red
        vtch = grahamCH vt
    chp _ _ = mempty


-- |Show coordinates as text above the convex hull points.
convexHPText :: Diag
convexHPText = Diag chpt
  where
    chpt p (Object vt)
      | showCoordText p =
          position $
            zip vtchf
                (pointToTextCoord <$> vtchf) # translate (r2 (0, 10))
      | otherwise = mempty
      where
        vtchf = grahamCH vt
    chpt _ _ = mempty


-- |Create a diagram which shows the lines along the convex hull
-- points.
convexHLs :: Diag
convexHLs = Diag chl
  where
    chl _ (Object []) = mempty
    chl _ (Object vt) =
      (strokeTrail
       . fromVertices
       . flip (++) [head $ grahamCH vt]
       . grahamCH
       $ vt)
      # moveTo (head $ grahamCH vt) # lc red
    chl _ _ = mempty


-- |Create list of diagrama which describe the lines along points of a half
-- convex hull, for each iteration of the algorithm. Which half is chosen
-- depends on the input.
convexHStepsLs :: Diag
convexHStepsLs = GifDiag chs
  where
    chs _ col f vt =
      fmap mkChDiag (f vt)
      where
        mkChDiag vt' =
          (strokeTrail . fromVertices $ vt') # moveTo (head vt') # lc col


-- |Create a diagram that shows all squares of the RangeSearch algorithm
-- from the quad tree.
squares :: Diag
squares = Diag f
  where
    f _ (Object []) = mempty
    f p (Object vt) =
      mconcat
      $ (\((xmin, xmax), (ymin, ymax)) -> rect (xmax - xmin) (ymax - ymin)
        # moveTo (p2 ((xmax + xmin) / 2, (ymax + ymin) / 2)) # lw ultraThin)
      <$> (quadTreeSquares (xDimension p, yDimension p)
        . quadTree vt
        $ (xDimension p, yDimension p))
    f _ _ = mempty


-- |Draw the squares of the kd-tree.
kdSquares:: Diag
kdSquares = Diag f
  where
    f _ (Object []) = mempty
    f p (Object vt) =
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


-- |Get the quad tree corresponding to the given points and diagram properties.
qt :: [PT] -> DiagProp -> QuadTree PT
qt vt p = quadTree vt (xDimension p, yDimension p)


-- |Create a diagram that shows a single square of the RangeSearch algorithm
-- from the quad tree in red, according to the given path in quadPath.
quadPathSquare :: Diag
quadPathSquare = Diag f
  where
    f _ (Object []) = mempty
    f p (Object vt) =
      (\((xmin, xmax), (ymin, ymax)) -> rect (xmax - xmin) (ymax - ymin)
        # moveTo (p2 ((xmax + xmin) / 2,(ymax + ymin) / 2)) # lw thin # lc red)
      (getSquare (stringToQuads (quadPath p)) (qt vt p, []))
      where
        getSquare :: [Either Quad Orient] -> Zipper PT -> Square
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
      (\((xmin, xmax), (ymin, ymax)) -> rect (xmax - xmin) (ymax - ymin)
        # moveTo (p2 ((xmax + xmin) / 2,(ymax + ymin) / 2)) # lw thick # lc col)
      <$> getSquares (stringToQuads (quadPath p)) (qt vt p, [])
      where
        getSquares :: [Either Quad Orient] -> Zipper PT -> [Square]
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
    f _ (Object []) = mempty
    f p (Object vt) =
      prettyRoseTree (quadTreeToRoseTree
                      . flip getCurQT (qt vt p, [])
                      . stringToQuads
                      . quadPath
                      $ p)
      where
        getCurQT :: [Either Quad Orient] -> Zipper PT -> Zipper PT
        getCurQT [] z = z
        getCurQT (q:qs) z = case q of
          Right x -> getCurQT qs (fromMaybe z (findNeighbor x z))
          Left x  -> getCurQT qs (fromMaybe z (goQuad x z))
        prettyRoseTree :: Tree String -> Diagram Cairo R2
        prettyRoseTree tree =
          renderTree (\n -> case head n of
                       '*' -> (text n # fontSizeL 5.0)
                              <> rect 50.0 20.0 # fc red
                       _   -> (text n # fontSizeL 5.0)
                              <> rect 50.0 20.0 # fc white)
                     (~~)
                     (symmLayout' (with & slHSep .~ 60 & slVSep .~ 40) tree)
          # scale 2 # alignT # bg white
    f _ _  = mempty


-- |Creates a Diagram that shows an XAxis which is bound
-- by the dimensions given in xDimension from DiagProp.
xAxis :: Diag
xAxis =
  Diag hRule
  <> Diag segments
  <> Diag labels
  where
    hRule p _ =
      arrowAt (p2 (diagXmin p, if diagYmin p <= 0 then 0 else diagYmin p))
              (r2 (diagWidth p, 0))
    segments p _ =
      hcat' (with & sep .~ squareSize p)
            (replicate (floor . (/) (diagWidth p) $ squareSize p)
                       (vrule 10))
      # moveTo (p2 (diagXmin p, if diagYmin p <= 0 then 0 else diagYmin p))
    labels p _ =
      position $
        zip (mkPoint <$> xs)
            ((\x -> (text . show $ x) # scale 10) <$> xs)
      where
        xs :: [Int]
        xs = take (floor . (/) (diagWidth p) $ squareSize p)
                  (iterate (+(floor . squareSize $ p)) (floor . diagXmin $ p))
        mkPoint x = p2 (fromIntegral x,
                        -15 + (if diagYmin p <= 0 then 0 else diagYmin p))


-- |Creates a Diagram that shows an YAxis which is bound
-- by the dimensions given in yDimension from DiagProp.
yAxis :: Diag
yAxis =
  Diag vRule
  <> Diag segments
  <> Diag labels
  where
    vRule p _ =
      arrowAt (p2 (if diagXmin p <= 0 then 0 else diagXmin p, diagYmin p))
              (r2 (0, diagHeight p))
    segments p _ =
      vcat' (with & sep .~ squareSize p)
            (replicate (floor . (/) (diagHeight p) $ squareSize p)
                       (hrule 10))
      # alignB
      # moveTo (p2 (if diagXmin p <= 0 then 0 else diagXmin p, diagYmin p))
    labels p _ =
      position $
        zip (mkPoint <$> ys)
            ((\x -> (text . show $ x) # scale 10) <$> ys)
        where
          ys :: [Int]
          ys = take (floor . (/) (diagHeight p) $ squareSize p)
                    (iterate (+(floor . squareSize $ p)) (floor . diagYmin $ p))
          mkPoint y = p2 (-15 + (if diagXmin p <= 0 then 0 else diagXmin p),
                          fromIntegral y)


-- |Creates a Diagram that shows a white rectangle which is a little
-- bit bigger than both X and Y axis dimensions from DiagProp.
whiteRectB :: Diag
whiteRectB = Diag rect'
  where
    rect' p _ =
      whiteRect (diagWidth p + (diagWidth p / 10))
                (diagHeight p + (diagHeight p / 10))
      # moveTo (p2 (diagWidthOffset p, diagHeightOffset p))
      where


-- |Create a white rectangle with the given width and height.
whiteRect :: Double -> Double -> Diagram Cairo R2
whiteRect x y = rect x y # lwG 0.00 # bg white


-- |Create a grid across the whole diagram with squares of the
-- given size in DiagProp.
grid :: Diag
grid = Diag xGrid <> Diag yGrid
  where
    yGrid p _
      | haveGrid p =
          hcat' (with & sep .~ squareSize p)
                (replicate (floor . (/) (diagWidth p) $ squareSize p)
                           (vrule $ diagHeight p))
          # moveTo (p2 (diagXmin p, diagHeightOffset p)) # lw ultraThin
      | otherwise = mempty
    xGrid p _
      | haveGrid p =
          vcat' (with & sep .~ squareSize p)
                (replicate (floor . (/) (diagHeight p) $ squareSize p)
                           (hrule $ diagWidth p))
          # alignB # moveTo (p2 (diagWidthOffset p, diagYmin p)) # lw ultraThin
      | otherwise = mempty


plotterBG :: Diag
plotterBG = mconcat [xAxis, yAxis, grid, whiteRectB]
