{-# OPTIONS_HADDOCK ignore-exports #-}

module LinearAlgebra.Vector where

import Data.Vector.Class


type Angle = Double


-- |Checks whether the Coordinates are in a given dimension.
inRange :: (Double, Double) -- ^ X dimension
        -> (Double, Double) -- ^ Y dimension
        -> (Double, Double) -- ^ Coordinates
        -> Bool             -- ^ result
inRange (xlD, xuD) (ylD, yuD) (x,y)
  = x <= xuD && x >= xlD && y <= yuD && y >= ylD


-- |Get the angle between two vectors in degrees.
getAngle :: (Vector v) => v -> v -> Angle
getAngle a b = (*) 180.0                    .
                 flip (/) pi                .
                 acos                       .
                 flip (/) (vmag a * vmag b) .
                 vdot a                     $
                 b
