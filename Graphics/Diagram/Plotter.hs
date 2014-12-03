{-# OPTIONS_HADDOCK ignore-exports #-}

module Graphics.Diagram.Plotter where

import Data.Monoid
import Diagrams.Backend.Cairo
import Diagrams.Prelude hiding ((<>))
import Graphics.Diagram.Core


-- |Creates a Diagram that shows the coordinates from the points
-- as dots. The points and thickness of the dots can be controlled
-- via DiagProp.
coordPoints :: Diag
coordPoints = Diag cp
  where
    cp p (Object vt) = drawP vt (dotSize p) # fc black # lc black
    cp p (Objects vts) = drawP (concat vts) (dotSize p) # fc black # lc black


-- |Show coordinates as text above all points.
coordPointsText :: Diag
coordPointsText = Diag cpt
  where
    cpt p (Object vt)   = drawT vt p
    cpt p (Objects vts) = drawT (concat vts) p
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
    hRule p _ = arrowAt (p2 (diagXmin p, if diagYmin p <= 0 then 0 else diagYmin p))
                        (r2 (diagWidth p, 0))
    segments p _ =  hcat' (with & sep .~ squareSize p)
                          (replicate (floor . (/) (diagWidth p) $ squareSize p)
                                     (vrule 10))
      # moveTo (p2 (diagXmin p, if diagYmin p <= 0 then 0 else diagYmin p))
    labels p _ = position . zip (mkPoint <$> xs)
                                $ ((\x -> (text . show $ x) # scale 10) <$> xs)
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
    vRule p _ = arrowAt (p2 (if diagXmin p <= 0 then 0 else diagXmin p, diagYmin p))
                        (r2 (0, diagHeight p))
    segments p _ = vcat' (with & sep .~ squareSize p)
                         (replicate (floor . (/) (diagHeight p) $ squareSize p)
                                    (hrule 10))
      # alignB
      # moveTo (p2 (if diagXmin p <= 0 then 0 else diagXmin p, diagYmin p))
    labels p _ = position . zip (mkPoint <$> ys)
                                $ ((\x -> (text . show $ x) # scale 10) <$> ys)
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
    rect' p _ = whiteRect (diagWidth p + (diagWidth p / 10))
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
      | haveGrid p = hcat' (with & sep .~ squareSize p)
                           (replicate (floor . (/) (diagWidth p) $ squareSize p)
                                      (vrule $ diagHeight p))
          # moveTo (p2 (diagXmin p, diagHeightOffset p)) # lw ultraThin
      | otherwise = mempty
    xGrid p _
      | haveGrid p = vcat' (with & sep .~ squareSize p)
                           (replicate (floor . (/) (diagHeight p) $ squareSize p)
                                      (hrule $ diagWidth p))
          # alignB # moveTo (p2 (diagWidthOffset p, diagYmin p)) # lw ultraThin
      | otherwise = mempty


plotterBG :: Diag
plotterBG = mconcat [xAxis, yAxis, grid, whiteRectB]
