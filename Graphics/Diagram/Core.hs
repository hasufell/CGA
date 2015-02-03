{-# OPTIONS_HADDOCK ignore-exports #-}

module Graphics.Diagram.Core where

import Algebra.Vector
import Diagrams.Backend.Cairo
import Diagrams.Prelude
import MyPrelude


-- |Represents a Cairo Diagram. This allows us to create multiple
-- diagrams with different algorithms but based on the same
-- coordinates and common properties.
data Diag =
  Diag
  {
    mkDiag :: DiagProp
           -> [[PT]]
           -> Diagram Cairo R2
  }
  | GifDiag
  {
    mkGifDiag :: DiagProp
              -> Colour Double
              -> ([PT] -> [[PT]])
              -> [PT]
              -> [Diagram Cairo R2]
  }
  | EmptyDiag (Diagram Cairo R2)


-- |Holds the properties for a Diagram, like thickness of 2d points etc.
-- This can also be seen as a context when merging multiple diagrams.
data DiagProp = MkProp {
  -- |The thickness of the dots.
  dotSize :: Double,
  -- |The dimensions of the x-axis.
  xDimension :: (Double, Double),
  -- |The dimensions of the y-axis.
  yDimension :: (Double, Double),
  -- |Algorithm to use.
  algo :: Int,
  -- |If we want to show the grid.
  haveGrid :: Bool,
  -- |If we want to show the coordinates as text.
  showCoordText :: Bool,
  -- |Square size used to show the grid and x/y-axis.
  squareSize :: Double,
  -- |The path to a quad in the quad tree.
  quadPath :: String,
  -- |The square for the kd-tree range search.
  rangeSquare :: Square
}


instance Def DiagProp where
  def = diagDefaultProp


instance Monoid Diag where
  mempty = EmptyDiag mempty
  mappend d1@(Diag {}) d2@(Diag {}) = Diag g
    where
      g p obj = mkDiag d1 p obj <> mkDiag d2 p obj
  mappend d1@(GifDiag {}) d2@(Diag {}) = GifDiag g
    where
      g p col f vt = mkGifDiag d1 p col f vt ++ [mkDiag d2 p [vt]]
  mappend d1@(Diag {}) d2@(GifDiag {}) = GifDiag g
    where
      g p col f vt = mkDiag d2 p [vt] : mkGifDiag d1 p col f vt
  mappend d1@(GifDiag {}) d2@(GifDiag {}) = GifDiag g
    where
      g p col f vt = mkGifDiag d1 p col f vt ++ mkGifDiag d2 p col f vt
  mappend (EmptyDiag _) g = g
  mappend g (EmptyDiag _) = g

  mconcat = foldr mappend mempty


-- |The default properties of the Diagram.
diagDefaultProp :: DiagProp
diagDefaultProp = MkProp 2 (0,500) (0,500)
                    0 False False 50 "" ((0,500),(0,500))


-- |Extract the lower bound of the x-axis dimension.
diagXmin :: DiagProp -> Double
diagXmin = fst . xDimension


-- |Extract the upper bound of the x-axis dimension.
diagXmax :: DiagProp -> Double
diagXmax = snd . xDimension


-- |Extract the lower bound of the y-axis dimension.
diagYmin :: DiagProp -> Double
diagYmin = fst . yDimension


-- |Extract the upper bound of the y-axis dimension.
diagYmax :: DiagProp -> Double
diagYmax = snd . yDimension


-- |The full width of the x dimension.
diagWidth :: DiagProp -> Double
diagWidth p = diagXmax p - diagXmin p


-- |The full height of the y dimension.
diagHeight :: DiagProp -> Double
diagHeight p = diagYmax p - diagYmin p


-- |The offset on the x-axis to move the grid and the white rectangle
-- to the right place.
diagWidthOffset :: DiagProp -> Double
diagWidthOffset p = diagXmin p + (diagWidth p / 2)


-- |The offset on the y-axis to move the grid and the white rectangle
-- to the right place.
diagHeightOffset :: DiagProp -> Double
diagHeightOffset p = diagYmin p + (diagHeight p / 2)


-- |Returns the specified diagram if True is passed,
-- otherwise returns the empty diagram. This is just for convenience
-- to avoid if else constructs.
maybeDiag :: Bool -> Diag -> Diag
maybeDiag b d
  | b         = d
  | otherwise = mempty


filterValidPT :: DiagProp -> [PT] -> [PT]
filterValidPT =
  filter
  . inRange
  . diagDimSquare


diagDimSquare :: DiagProp -> Square
diagDimSquare p = dimToSquare (xDimension p) $ yDimension p


-- |Draw a list of points.
drawP :: [PT]              -- ^ the points to draw
      -> Double            -- ^ dot size
      -> Diagram Cairo R2  -- ^ the resulting diagram
drawP [] _  = mempty
drawP vt ds =
  position (zip vt (repeat dot))
  where
    dot = circle ds :: Diagram Cairo R2


-- |Create a rectangle around a diagonal line, which has sw
-- as startpoint and nw as endpoint.
rectByDiagonal :: (Double, Double)  -- ^ sw point
               -> (Double, Double)  -- ^ nw point
               -> Diagram Cairo R2
rectByDiagonal (xmin, ymin) (xmax, ymax) =
  fromVertices [p2 (xmin, ymin)
                , p2 (xmax, ymin)
                , p2 (xmax, ymax)
                , p2 (xmin, ymax)
                , p2 (xmin, ymin)
               ]


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
