{-# OPTIONS_HADDOCK ignore-exports #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Test.Vector where

import Algebra.Vector
import Control.Applicative
import Control.Arrow
{- import Control.Monad -}
import Diagrams.TwoD.Types
import Test.QuickCheck


instance Arbitrary R2 where
  arbitrary = curry r2 <$> arbitrary <*> arbitrary


instance Arbitrary P2 where
  arbitrary = curry p2 <$> arbitrary <*> arbitrary


-- the point describing the lower left corner of the square
-- must be part of the square
inRangeProp1 :: Square -> Bool
inRangeProp1 sq@((x1, y1), _) =
  inRange sq (p2 (x1, y1))


-- the point describing the upper right corner of the square
-- must be part of the square
inRangeProp2 :: Square -> Bool
inRangeProp2 sq@(_, (x2, y2)) =
  inRange sq (p2 (x2, y2))


-- the point describing the upper left corner of the square
-- must be part of the square
inRangeProp3 :: Square -> Bool
inRangeProp3 sq@((x1, _), (_, y2)) =
  inRange sq (p2 (x1, y2))


-- the point describing the lower right corner of the square
-- must be part of the square
inRangeProp4 :: Square -> Bool
inRangeProp4 sq@((_, y1), (x2, _)) =
  inRange sq (p2 (x2, y1))


-- generating random points within the square
inRangeProp5 :: Square -> Positive Double -> Positive Double -> Bool
inRangeProp5 sq@((x1, y1), (x2, y2)) (Positive a) (Positive b) =
  inRange sq (p2 (x1 + ((x2 - x1) / (a + 1)), y1 + ((y2 - y1) / (b + 1))))


-- apply id function on the point
onPTProp1 :: PT -> Bool
onPTProp1 pt = onPT id pt == pt


-- add a random value to the point coordinates
onPTProp2 :: PT -> Positive R2 -> Bool
onPTProp2 pt (Positive (R2 rx ry))
  = onPT (\(x, y) -> (x + rx, y + ry)) pt /= pt


-- angle between two vectors both on the x-axis must be 0
getAngleProp1 :: Positive Vec -> Positive Vec -> Bool
getAngleProp1 (Positive (R2 x1 _)) (Positive (R2 x2 _))
  = getAngle (R2 x1 0) (R2 x2 0) == 0


-- angle between two vectors both on the y-axis must be 0
getAngleProp2 :: Positive Vec -> Positive Vec -> Bool
getAngleProp2 (Positive (R2 _ y1)) (Positive (R2 _ y2))
  = getAngle (R2 0 y1) (R2 0 y2) == 0


-- angle between two vectors both on the x-axis but with opposite direction
-- must be pi
getAngleProp3 :: Positive Vec -> Positive Vec -> Bool
getAngleProp3 (Positive (R2 x1 _)) (Positive (R2 x2 _))
  = getAngle (R2 (negate x1) 0) (R2 x2 0) == pi


-- angle between two vectors both on the y-axis but with opposite direction
-- must be pi
getAngleProp4 :: Positive Vec -> Positive Vec -> Bool
getAngleProp4 (Positive (R2 _ y1)) (Positive (R2 _ y2))
  = getAngle (R2 0 (negate y1)) (R2 0 y2) == pi


-- angle between vector in x-axis direction and y-axis direction must be
-- p/2
getAngleProp5 :: Positive Vec -> Positive Vec -> Bool
getAngleProp5 (Positive (R2 x1 _)) (Positive (R2 _ y2))
  = getAngle (R2 x1 0) (R2 0 y2) == pi / 2


-- commutative
getAngleProp6 :: Positive Vec -> Positive Vec -> Bool
getAngleProp6 (Positive v1) (Positive v2)
  = getAngle v1 v2 == getAngle v2 v1


-- commutative
scalarProdProp1 :: Vec -> Vec -> Bool
scalarProdProp1 v1 v2 = v1 `scalarProd` v2 == v2 `scalarProd` v1


-- distributive, avoid doubles as we get messed up precision
scalarProdProp2 :: (Int, Int) -> (Int, Int) -> (Int, Int) -> Bool
scalarProdProp2 v1 v2 v3 =
  v1' `scalarProd` (v2' + v3')
  ==
  (v1' `scalarProd` v2') + (v1' `scalarProd` v3')
  where
    [v1', v2', v3'] = fmap (r2 . (fromIntegral *** fromIntegral)) [v1, v2, v3]


-- bilinear, avoid doubles as we get messed up precision
scalarProdProp3 :: Int -> (Int, Int) -> (Int, Int) -> (Int, Int) -> Bool
scalarProdProp3 r v1 v2 v3 =
  v1' `scalarProd` (scalarMul r' v2' + v3')
  ==
  r' * (v1' `scalarProd` v2') + (v1' `scalarProd` v3')
  where
    [v1', v2', v3'] = fmap (r2 . (fromIntegral *** fromIntegral)) [v1, v2, v3]
    r'  = fromIntegral r


-- scalar multiplication
scalarProdProp4 :: Int -> Int -> (Int, Int) -> (Int, Int) -> Bool
scalarProdProp4 s1 s2 v1 v2
  = scalarMul s1' v1' `scalarProd` scalarMul s2' v2'
    ==
    s1' * s2' * (v1' `scalarProd` v2')
    where
      [v1', v2'] = fmap (r2 . (fromIntegral *** fromIntegral)) [v1, v2]
      s1' = fromIntegral s1
      s2' = fromIntegral s2


-- orthogonal
scalarProdProp5 :: Positive Vec -> Positive Vec -> Bool
scalarProdProp5 (Positive (R2 x1 _)) (Positive (R2 _ y2))
  = scalarProd (R2 x1 0) (R2 0 y2) == 0
