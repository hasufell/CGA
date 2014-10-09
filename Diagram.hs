{-# OPTIONS_HADDOCK ignore-exports #-}

module Diagram (t,
                dX,
                dY,
                alg,
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


-- |Represents a Cairo Diagram. This allows us to create multiple
-- diagrams with different algorithms but based on the same
-- coordinates and common properties.
data Diag = Diag {
  mkDiag :: DiagProp
         -> [PT]
         -> Diagram Cairo R2
}


-- |Holds the properties for a Diagram, like thickness of 2d points etc.
data DiagProp = MkProp {
  -- |The thickness of the dots.
  t :: Double,
  -- |The dimensions of the x-axis.
  dX :: Coord,
  -- |The dimensions of the y-axis.
  dY :: Coord,
  -- |Algorithm to use.
  alg :: Int
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
defaultProp = MkProp 2 (0,500) (0,500) 0


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
coordPoints = Diag f
  where
    f p vt
      = position (zip (filter (inRange (dX p) (dY p)) $ vt)
                      (repeat dot))
            where
              -- a dot itself is a diagram
              dot = (circle $ t p :: Diagram Cairo R2) # fc black


-- |Create a diagram which shows the points of the convex hull.
convexHullPoints :: Diag
convexHullPoints = Diag f
  where
    f p vt
      = position (zip (filter (inRange (dX p) (dY p)) $ vtch)
                      (repeat dot))
            where
              -- a dot itself is a diagram
              dot = (circle $ t p :: Diagram Cairo R2) # fc red # lc red
              vtch = grahamGetCH vt


-- |Create a diagram which shows the lines along the convex hull
-- points.
convexHullLines :: Diag
convexHullLines = Diag f
  where
    f _ [] = mempty
    f _ vt
      = (strokeTrail                       .
         fromVertices                      .
         flip (++) [head $ grahamGetCH vt] .
         grahamGetCH                       $
         vt
        ) # moveTo (head $ grahamGetCH vt) # lc red


-- |Same as showConvexHullLines, except that it returns an array
-- of diagrams with each step of the algorithm.
convexHullLinesInterval :: DiagProp -> [PT] -> [Diagram Cairo R2]
convexHullLinesInterval _ xs =
  fmap g (grahamGetCHSteps xs)
    where
      g vt
        = (strokeTrail        .
           fromVertices       $
           vt
          ) # moveTo (head vt) # lc red


-- |Creates a Diagram that shows an XAxis which is bound
-- by the dimensions given in xD from DiagProp.
xAxis :: Diag
xAxis = Diag f
  where
    f p _ = (strokeTrail . fromVertices $ [p2 (xlD p,0), p2 (xuD p, 0)]) # moveTo (p2 (xlD p,0))


-- |Creates a Diagram that shows an YAxis which is bound
-- by the dimensions given in yD from DiagProp.
yAxis :: Diag
yAxis = Diag f
  where
    f p _ = (strokeTrail . fromVertices $ [p2 (0, ylD p), p2 (0, yuD p)]) # moveTo (p2 (0, ylD p))


-- |Creates a Diagram that shows a white rectangle which is a little
-- bit bigger as both X and Y axis dimensions from DiagProp.
whiteRectB :: Diag
whiteRectB = Diag f
  where
    f p _ = whiteRect (w' + 50) (h' + 50) # moveTo (p2 (w' / 2, h' / 2))
      where
        w' = xuD p - xlD p
        h' = yuD p - ylD p


-- |Create the Diagram from the points.
diag :: DiagProp -> [PT] -> Diagram Cairo R2
diag p = case alg p of
  0 -> mkDiag
         (mconcat [coordPoints, xAxis, yAxis, whiteRectB])
         p
  1 -> mkDiag
         (mconcat $
           [convexHullPoints, convexHullLines, coordPoints,
            xAxis, yAxis, whiteRectB])
         p
  _ -> mempty


-- |Create the Diagram from a String which is supposed to be the contents
-- of an obj file.
diagS :: DiagProp -> String -> Diagram Cairo R2
diagS p mesh
  = (diag p      .
      meshToArr $
      mesh) # bg white


-- |Return a list of tuples used by 'gifMain' to generate an animated gif.
gifDiag :: DiagProp -> [PT] -> [(Diagram Cairo R2, GifDelay)]
gifDiag p xs = fmap (\x -> (x, 100))                      .
                 fmap (\x -> x <> g)                      .
                 flip (++)
                   [mkDiag (convexHullLines `mappend`
                            convexHullPoints) p xs]   $
                 (convexHullLinesInterval p xs)
                   where
                     g = mconcat                      .
                           fmap (\x -> mkDiag x p xs) $
                           [coordPoints,
                            xAxis,
                            yAxis,
                            whiteRectB]


-- |Same as gifDiag, except that it takes a string containing the
-- mesh file content instead of the the points.
gifDiagS :: DiagProp -> String -> [(Diagram Cairo R2, GifDelay)]
gifDiagS p = gifDiag p .
               meshToArr


-- |Create a white rectangle with the given width and height.
whiteRect :: Double -> Double -> Diagram Cairo R2
whiteRect x y = rect x y # lwG 0.00 # bg white
