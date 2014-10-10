{-# OPTIONS_HADDOCK ignore-exports #-}

module Diagram (t,
                dX,
                dY,
                alg,
                gd,
                ct,
                defaultProp,
                diag,
                diagS,
                gifDiag,
                gifDiagS,
                whiteRect) where

import Algorithms.ConvexHull
import Codec.Picture.Gif
import Class.Defaults
import Diagrams.Backend.Cairo
import Diagrams.Prelude
import LinearAlgebra.Vector
import Parser.Meshparser


type MeshString = String


-- |Represents a Cairo Diagram. This allows us to create multiple
-- diagrams with different algorithms but based on the same
-- coordinates and common properties.
data Diag = Diag {
  mkDiag :: DiagProp
         -> [PT]
         -> Diagram Cairo R2
}


-- |Holds the properties for a Diagram, like thickness of 2d points etc.
-- This can also be seen as a context when merging multiple diagrams.
data DiagProp = MkProp {
  -- |The thickness of the dots.
  t :: Double,
  -- |The dimensions of the x-axis.
  dX :: Coord,
  -- |The dimensions of the y-axis.
  dY :: Coord,
  -- |Algorithm to use.
  alg :: Int,
  -- |If we want to show the grid.
  gd :: Bool,
  -- |If we want to show the coordinates as text.
  ct :: Bool,
  -- |Square size used to show the grid and x/y-axis.
  sqS :: Double
}


instance Def DiagProp where
  def = defaultProp


instance Monoid Diag where
  mempty = Diag (\_ _ -> mempty)
  mappend d1 d2 = Diag g
    where
      g p vt = mkDiag d1 p vt <> mkDiag d2 p vt
  mconcat = foldr mappend mempty


-- |The default properties of the Diagram.
defaultProp :: DiagProp
defaultProp = MkProp 2 (0,500) (0,500) 0 False False 50


-- |Extract the lower bound of the x-axis dimension.
xlD :: DiagProp -> Double
xlD = fst . dX


-- |Extract the upper bound of the x-axis dimension.
xuD :: DiagProp -> Double
xuD = snd . dX


-- |Extract the lower bound of the y-axis dimension.
ylD :: DiagProp -> Double
ylD = fst . dY


-- |Extract the upper bound of the y-axis dimension.
yuD :: DiagProp -> Double
yuD = snd . dY


-- |Creates a Diagram that shows the coordinates from the points
-- as dots. The points and thickness of the dots can be controlled
-- via DiagProp.
coordPoints :: Diag
coordPoints = Diag cp
  where
    cp p vt =
      position (zip (filter (inRange (dX p) (dY p)) $ vt)
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
    cpt _ vt =
      position $
        zip vt (pointToTextCoord <$> vt) # translate (r2 (0, 10))


-- |Create a diagram which shows the points of the convex hull.
convexHullPoints :: Diag
convexHullPoints = Diag chp
  where
    chp p vt =
      position (zip (filter (inRange (dX p) (dY p)) $ vtch)
        (repeat dot))
      where
        dot = (circle $ t p :: Diagram Cairo R2) # fc red # lc red
        vtch = grahamGetCH vt


-- |Show coordinates as text above the convex hull points.
convexHullPointsText :: Diag
convexHullPointsText = Diag chpt
  where
    chpt _ vt =
      position $
        zip vtch
            (pointToTextCoord <$> vtch) # translate (r2 (0, 10))
      where
        vtch = grahamGetCH vt


-- |Create a diagram which shows the lines along the convex hull
-- points.
convexHullLines :: Diag
convexHullLines = Diag chl
  where
    chl _ [] = mempty
    chl p vt =
      (strokeTrail                           .
          fromVertices                       .
          flip (++) [head $ grahamGetCH vtf] .
          grahamGetCH                        $
          vtf)                          #
        moveTo (head $ grahamGetCH vtf) #
        lc red
      where
        vtf = filter (inRange (dX p) (dY p)) vt


-- |Same as showConvexHullLines, except that it returns an array
-- of diagrams with each step of the algorithm.
-- Unfortunately this is very difficult to implement as a Diag (TODO).
convexHullLinesInterval :: DiagProp -> [PT] -> [Diagram Cairo R2]
convexHullLinesInterval p xs =
  fmap mkChDiag (grahamGetCHSteps xs)
  where
    mkChDiag vt =
      (strokeTrail         .
          fromVertices     $
          vtf)            #
        moveTo (head vtf) #
        lc red
      where
        vtf = filter (inRange (dX p) (dY p)) vt


-- |Creates a Diagram that shows an XAxis which is bound
-- by the dimensions given in xD from DiagProp.
xAxis :: Diag
xAxis =
  (Diag hRule)      `mappend`
    (Diag segments) `mappend`
    (Diag labels)
  where
    hRule p _ =
      arrowAt (p2 (xlD p,0)) (r2 (xuD p, 0)) # moveTo (p2 (xlD p,0))
    segments p _ =
      hcat' (with & sep .~ (sqS p))
            (take (floor . (/) (xuD p - xlD p) $ (sqS p)) .
              repeat $ (vrule 10)) #
      moveTo (p2 (xlD p,0))
    labels p _ =
      position $
        zip (mkPoint <$> xs)
            ((\x -> (flip (<>) (square 1 # lw none) .
              text . show $ x) # scale 10) <$> xs)
      where
        xs :: [Int]
        xs = take (floor . (/) (xuD p - xlD p) $ (sqS p))
                  (iterate (+(floor . sqS $ p)) 0)
        mkPoint x = p2 (fromIntegral x, -15)


-- |Creates a Diagram that shows an YAxis which is bound
-- by the dimensions given in yD from DiagProp.
yAxis :: Diag
yAxis =
  (Diag vRule)      `mappend`
    (Diag segments) `mappend`
    (Diag labels)
  where
    vRule p _ =
      arrowAt (p2 (0, ylD p)) (r2 (0, yuD p)) # moveTo (p2 (0, ylD p))
    segments p _ =
      vcat' (with & sep .~ (sqS p))
            (take (floor . (/) (yuD p - ylD p) $ (sqS p)) .
              repeat $ (hrule 10)) #
      alignB                       #
      moveTo (p2 (0, (ylD p)))
    labels p _ =
      position $
        zip (mkPoint <$> ys)
            ((\x -> (flip (<>) (square 1 # lw none) .
              text . show $ x) # scale 10) <$> ys)
        where
          ys :: [Int]
          ys = take (floor . (/) (yuD p - ylD p) $ (sqS p))
                    (iterate (+(floor . sqS $ p)) 0)
          mkPoint y = p2 (-15, fromIntegral y)


-- |Creates a Diagram that shows a white rectangle which is a little
-- bit bigger as both X and Y axis dimensions from DiagProp.
whiteRectB :: Diag
whiteRectB = Diag rect'
  where
    rect' p _ = whiteRect (w' + 50) (h' + 50) # moveTo (p2 (w' / 2, h' / 2))
      where
        w' = xuD p - xlD p
        h' = yuD p - ylD p


-- |Create the Diagram from the points.
diag :: DiagProp -> [PT] -> Diagram Cairo R2
diag p = case alg p of
  0 ->
    mkDiag
      (mconcat [if ct p then coordPointsText else mempty,
        coordPoints, xAxis, yAxis,
        (if gd p then grid else mempty),whiteRectB])
      p
  1 ->
    mkDiag
      (mconcat
        [if ct p then convexHullPointsText else mempty,
        convexHullPoints, convexHullLines,
        coordPoints, xAxis, yAxis,
        (if gd p then grid else mempty), whiteRectB])
      p
  _ -> mempty


-- |Create the Diagram from a String which is supposed to be the contents
-- of an obj file.
diagS :: DiagProp -> MeshString -> Diagram Cairo R2
diagS p mesh =
  (diag p       .
      meshToArr $
      mesh) #
    bg white


-- |Return a list of tuples used by 'gifMain' to generate an animated gif.
gifDiag :: DiagProp -> [PT] -> [(Diagram Cairo R2, GifDelay)]
gifDiag p xs =
  fmap (\x -> (x, 100))                      .
    fmap (\x -> x <> nonChDiag)              .
    flip (++)
      [mkDiag (convexHullLines `mappend`
      convexHullPoints) p xs]                $
    (convexHullLinesInterval p xs)
  where
    -- add the x-axis and the other default stuff
    nonChDiag =
      mconcat                      .
        fmap (\x -> mkDiag x p xs) $
        [coordPoints,
        xAxis,
        yAxis,
        whiteRectB]


-- |Same as gifDiag, except that it takes a string containing the
-- mesh file content instead of the the points.
gifDiagS :: DiagProp -> MeshString -> [(Diagram Cairo R2, GifDelay)]
gifDiagS p = gifDiag p . meshToArr


-- |Create a white rectangle with the given width and height.
whiteRect :: Double -> Double -> Diagram Cairo R2
whiteRect x y = rect x y # lwG 0.00 # bg white


-- |Create a grid across the whole diagram with squares of the
-- given size in DiagProp.
grid :: Diag
grid = Diag xGrid `mappend` Diag yGrid
  where
    yGrid p _ =
      hcat' (with & sep .~ (sqS p))
            (take (floor . (/) (xuD p - xlD p) $ (sqS p)) .
              repeat $ (vrule $ xuD p - xlD p))   #
      moveTo (p2 (xlD p, (yuD p - ylD p) / 2))    #
      lw ultraThin
    xGrid p _ =
      vcat' (with & sep .~ (sqS p))
            (take (floor . (/) (yuD p - ylD p) $ (sqS p)) .
              repeat $ (hrule $ yuD p - ylD p))  #
      alignB                                     #
      moveTo (p2 ((xuD p - xlD p) / 2, ylD p))   #
      lw ultraThin
