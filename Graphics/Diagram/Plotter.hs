{-# OPTIONS_HADDOCK ignore-exports #-}

module Graphics.Diagram.Plotter where

import Algebra.Vector
import Algebra.VectorTypes
import Algorithms.ConvexHull.GrahamScan
import Diagrams.Backend.Cairo
import Diagrams.Prelude
import Graphics.Diagram.Types


-- |Creates a Diagram that shows the coordinates from the points
-- as dots. The points and thickness of the dots can be controlled
-- via DiagProp.
coordPoints :: Diag
coordPoints = Diag cp
  where
    cp p vt =
      position (zip (filter (inRange (dX p) (dY p)) vt)
        (repeat dot))
      where
        dot = (circle $ t p :: Diagram Cairo R2) # fc black


-- |Creates a Diagram from a point that shows the coordinates
-- in text format, such as "(1.0, 2.0)".
pointToTextCoord :: PT -> Diagram Cairo R2
pointToTextCoord pt =
  text ("(" ++ show x ++ ", " ++ show y ++ ")") # scale 10
  where
    (x, y) = unp2 pt


-- |Show coordinates as text above all points.
coordPointsText :: Diag
coordPointsText = Diag cpt
  where
    cpt p vt =
      position $
        zip vtf (pointToTextCoord <$> vtf) # translate (r2 (0, 10))
      where
        vtf = filter (inRange (dX p) (dY p)) vt


-- |Create a diagram which shows the points of the convex hull.
convexHullPoints :: Diag
convexHullPoints = Diag chp
  where
    chp p vt =
      position (zip (filter (inRange (dX p) (dY p)) vtch)
        (repeat dot))
      where
        dot = (circle $ t p :: Diagram Cairo R2) # fc red # lc red
        vtch = grahamCH vt


-- |Show coordinates as text above the convex hull points.
convexHullPointsText :: Diag
convexHullPointsText = Diag chpt
  where
    chpt p vt =
      position $
        zip vtchf
            (pointToTextCoord <$> vtchf) # translate (r2 (0, 10))
      where
        vtchf = grahamCH . filter (inRange (dX p) (dY p)) $ vt


-- |Create a diagram which shows the lines along the convex hull
-- points.
convexHullLines :: Diag
convexHullLines = Diag chl
  where
    chl _ [] = mempty
    chl p vt =
      (strokeTrail                           .
          fromVertices                       .
          flip (++) [head $ grahamCH vtf]    .
          grahamCH                           $
          vtf)                       #
        moveTo (head $ grahamCH vtf) #
        lc red
      where
        vtf = filter (inRange (dX p) (dY p)) vt


convexHullLinesIntervalLower :: DiagProp -> [PT] -> [Diagram Cairo R2]
convexHullLinesIntervalLower p xs =
  fmap mkChDiag (grahamGetLowerHullSteps xs)
  where
    mkChDiag vt =
      (strokeTrail         .
          fromVertices     $
          vtf)            #
        moveTo (head vtf) #
        lc orange
      where
        vtf = filter (inRange (dX p) (dY p)) vt


convexHullLinesIntervalUpper :: DiagProp -> [PT] -> [Diagram Cairo R2]
convexHullLinesIntervalUpper p xs =
  fmap mkChDiag (grahamGetUpperHullSteps xs)
  where
    mkChDiag vt =
      (strokeTrail         .
          fromVertices     $
          vtf)            #
        moveTo (head vtf) #
        lc purple
      where
        vtf = filter (inRange (dX p) (dY p)) vt


-- |Creates a Diagram that shows an XAxis which is bound
-- by the dimensions given in xD from DiagProp.
xAxis :: Diag
xAxis =
  Diag hRule      `mappend`
    Diag segments `mappend`
    Diag labels
  where
    hRule p _ =
      arrowAt (p2 (xmin p, if ymin p <= 0 then 0 else ymin p))
              (r2 (w' p, 0))
    segments p _ =
      hcat' (with & sep .~ sqS p)
            (replicate (floor . (/) (w' p) $ sqS p)
                       (vrule 10)) #
      moveTo (p2 (xmin p, if ymin p <= 0 then 0 else ymin p))
    labels p _ =
      position $
        zip (mkPoint <$> xs)
            ((\x -> (text . show $ x) # scale 10) <$> xs)
      where
        xs :: [Int]
        xs = take (floor . (/) (w' p) $ sqS p)
                  (iterate (+(floor . sqS $ p)) (floor . xmin $ p))
        mkPoint x = p2 (fromIntegral x,
                        -15 + (if ymin p <= 0 then 0 else ymin p))


-- |Creates a Diagram that shows an YAxis which is bound
-- by the dimensions given in yD from DiagProp.
yAxis :: Diag
yAxis =
  Diag vRule      `mappend`
    Diag segments `mappend`
    Diag labels
  where
    vRule p _ =
      arrowAt (p2 (if xmin p <= 0 then 0 else xmin p, ymin p))
              (r2 (0, h' p))
    segments p _ =
      vcat' (with & sep .~ sqS p)
            (replicate (floor . (/) (h' p) $ sqS p)
                       (hrule 10)) #
      alignB                       #
      moveTo (p2 (if xmin p <= 0 then 0 else xmin p, ymin p))
    labels p _ =
      position $
        zip (mkPoint <$> ys)
            ((\x -> (text . show $ x) # scale 10) <$> ys)
        where
          ys :: [Int]
          ys = take (floor . (/) (h' p) $ sqS p)
                    (iterate (+(floor . sqS $ p)) (floor . ymin $ p))
          mkPoint y = p2 (-15 + (if xmin p <= 0 then 0 else xmin p),
                          fromIntegral y)


-- |Creates a Diagram that shows a white rectangle which is a little
-- bit bigger than both X and Y axis dimensions from DiagProp.
whiteRectB :: Diag
whiteRectB = Diag rect'
  where
    rect' p _ =
      whiteRect (w' p + 50) (h' p + 50) #
        moveTo (p2 (wOff p, hOff p))
      where


-- |Create a white rectangle with the given width and height.
whiteRect :: Double -> Double -> Diagram Cairo R2
whiteRect x y = rect x y # lwG 0.00 # bg white


-- |Create a grid across the whole diagram with squares of the
-- given size in DiagProp.
grid :: Diag
grid = Diag xGrid `mappend` Diag yGrid
  where
    yGrid p _ =
      hcat' (with & sep .~ sqS p)
            (replicate (floor . (/) (w' p) $ sqS p)
                       (vrule $ h' p))  #
      moveTo (p2 (xmin p, hOff p))      #
      lw ultraThin
    xGrid p _ =
      vcat' (with & sep .~ sqS p)
            (replicate (floor . (/) (h' p) $ sqS p)
                       (hrule $ w' p))  #
      alignB                            #
      moveTo (p2 (wOff p, ymin p))      #
      lw ultraThin
      where
