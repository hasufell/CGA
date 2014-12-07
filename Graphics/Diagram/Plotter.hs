{-# OPTIONS_HADDOCK ignore-exports #-}

module Graphics.Diagram.Plotter where

import Data.Monoid
import Diagrams.Prelude hiding ((<>))
import Graphics.Diagram.Core


-- |All x coordinates separated by the squareSize on DiagProp.
xAxisPoints :: DiagProp -> [Double]
xAxisPoints p = takeWhile (< diagXmax p)
                  . iterate (+ squareSize p)
                  $ diagXmin p


-- |All y coordinates separated by the squareSize on DiagProp.
yAxisPoints :: DiagProp -> [Double]
yAxisPoints p = takeWhile (< diagYmax p)
                  . iterate (+ squareSize p)
                  $ diagYmin p


-- |Creates a Diagram that shows the coordinates from the points
-- as dots. The points and thickness of the dots can be controlled
-- via DiagProp.
coordPoints :: Diag
coordPoints = Diag f
  where
    f p vts  = drawP (concat vts) (relDotSize p) # fc black # lc black
    relDotSize p = dotSize p / 500 * ((diagWidth p + diagHeight p) / 2)


-- |Show coordinates as text above all points.
coordPointsText :: Diag
coordPointsText = Diag f
  where
    f p vts  = drawT (concat vts) p
    drawT [] _ = mempty
    drawT vt p
      | showCoordText p = position $ zip vt (pointToTextCoord <$> vt)
          # translate (r2 (0, 10))
      | otherwise = mempty


-- |Creates a Diagram that shows an XAxis which is bound
-- by the dimensions given in xDimension from DiagProp.
xAxis :: Diag
xAxis =
  Diag hRule
  <> Diag segments
  <> Diag labels
  where
    hRule p _ =
      arrowAt (p2 (diagXmin p, diagYminPos p))
              (r2 (diagWidth p, 0))
    segments p _ =
      mconcat
        . fmap (\x -> p2 (x, diagYminPos p - segY)
                   ~~ p2 (x, diagYminPos p + segY))
        $ xAxisPoints p
      where
        segY = diagWidth p / 100
    labels p _ =
      position
        . zip (mkPoint <$> xAxisPoints p)
        $ ((\x -> (text . show . floor $ x) # scale labelScale)
             <$> xAxisPoints p)
        where
          mkPoint x =
            p2 (x, labelOffset + diagYminPos p)
          labelScale = diagWidth p / 50
          labelOffset = negate (diagWidth p / 50 * 2)
    diagYminPos p = if diagYmin p <= 0 then 0 else diagYmin p


-- |Creates a Diagram that shows an YAxis which is bound
-- by the dimensions given in yDimension from DiagProp.
yAxis :: Diag
yAxis =
  Diag vRule
  <> Diag segments
  <> Diag labels
  where
    vRule p _ =
      arrowAt (p2 (diagXminPos p, diagYmin p))
              (r2 (0, diagHeight p))
    segments p _ =
      mconcat
        . fmap (\y -> p2 (diagXminPos p - segX, y)
                   ~~ p2 (diagXminPos p + segX, y))
        $ yAxisPoints p
      where
        segX = diagHeight p / 100
    labels p _ =
      position
        . zip (mkPoint <$> yAxisPoints p)
        $ ((\x -> (text . show . floor $ x) # scale labelScale)
             <$> yAxisPoints p)
        where
          mkPoint y =
            p2 (labelOffset + diagXminPos p, y)
          labelScale = diagHeight p / 50
          labelOffset = negate (diagHeight p / 50 * 2)
    diagXminPos p = if diagXmin p <= 0 then 0 else diagXmin p

-- |Creates a Diagram that shows a white rectangle which is a little
-- bit bigger than both X and Y axis dimensions from DiagProp.
whiteRectB :: Diag
whiteRectB = Diag rect'
  where
    rect' p _ = rect (diagWidth p + (diagWidth p / 10))
                     (diagHeight p + (diagHeight p / 10))
                  # lwG 0.00
                  # bg white
                  # moveTo (p2 (diagWidthOffset p, diagHeightOffset p))


-- |Create a grid across the whole diagram with squares of the
-- given size in DiagProp.
grid :: Diag
grid = Diag xGrid <> Diag yGrid
  where
    yGrid p _
      | haveGrid p =
          mconcat
            . fmap (\x -> p2 (x, diagYmin p)
                       ~~ p2 (x, diagYmax p) # lw ultraThin)
            $ xAxisPoints p
      | otherwise = mempty
    xGrid p _
      | haveGrid p =
          mconcat
            . fmap (\y -> p2 (diagXmin p, y)
                       ~~ p2 (diagXmax p, y) # lw ultraThin)
            $ yAxisPoints p
      | otherwise = mempty


plotterBG :: Diag
plotterBG = mconcat [xAxis, yAxis, grid, whiteRectB]
