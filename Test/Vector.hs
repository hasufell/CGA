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


inRangeProp1 :: Square -> Bool
inRangeProp1 sq@((x1, y1), _) =
  inRange sq (p2 (x1, y1))


inRangeProp2 :: Square -> Bool
inRangeProp2 sq@(_, (x2, y2)) =
  inRange sq (p2 (x2, y2))


inRangeProp3 :: Square -> Bool
inRangeProp3 sq@((x1, _), (_, y2)) =
  inRange sq (p2 (x1, y2))


inRangeProp4 :: Square -> Bool
inRangeProp4 sq@((_, y1), (x2, _)) =
  inRange sq (p2 (x2, y1))


inRangeProp5 :: Square -> Positive Double -> Positive Double -> Bool
inRangeProp5 sq@((x1, y1), (x2, y2)) (Positive a) (Positive b) =
  inRange sq (p2 (x1 + ((x2 - x1) / (a + 1)), y1 + ((y2 - y1) / (b + 1))))


onPTProp1 :: PT -> Bool
onPTProp1 pt = onPT id pt == pt


onPTProp2 :: PT -> Positive R2 -> Bool
onPTProp2 pt (Positive (R2 rx ry))
  = onPT (\(x, y) -> (x + rx, y + ry)) pt /= pt


getAngleProp1 :: Positive Vec -> Positive Vec -> Bool
getAngleProp1 (Positive (R2 x1 _)) (Positive (R2 x2 _))
  = getAngle (R2 x1 0) (R2 x2 0) == 0


getAngleProp2 :: Positive Vec -> Positive Vec -> Bool
getAngleProp2 (Positive (R2 _ y1)) (Positive (R2 _ y2))
  = getAngle (R2 0 y1) (R2 0 y2) == 0


getAngleProp3 :: Positive Vec -> Positive Vec -> Bool
getAngleProp3 (Positive (R2 x1 _)) (Positive (R2 x2 _))
  = getAngle (R2 (negate x1) 0) (R2 x2 0) == pi


getAngleProp4 :: Positive Vec -> Positive Vec -> Bool
getAngleProp4 (Positive (R2 _ y1)) (Positive (R2 _ y2))
  = getAngle (R2 0 (negate y1)) (R2 0 y2) == pi


getAngleProp5 :: Positive Vec -> Positive Vec -> Bool
getAngleProp5 (Positive (R2 x1 _)) (Positive (R2 _ y2))
  = getAngle (R2 x1 0) (R2 0 y2) == pi / 2


getAngleProp6 :: Positive Vec -> Positive Vec -> Bool
getAngleProp6 (Positive (R2 _ y1)) (Positive (R2 x2 _))
  = getAngle (R2 0 y1) (R2 x2 0) == pi / 2


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
    v1' = r2 . (fromIntegral *** fromIntegral) $ v1
    v2' = r2 . (fromIntegral *** fromIntegral) $ v2
    v3' = r2 . (fromIntegral *** fromIntegral) $ v3


-- bilinear, avoid doubles as we get messed up precision
scalarProdProp3 :: Int -> (Int, Int) -> (Int, Int) -> (Int, Int) -> Bool
scalarProdProp3 r v1 v2 v3 =
  v1' `scalarProd` (scalarMul r' v2' + v3')
  ==
  r' * (v1' `scalarProd` v2') + (v1' `scalarProd` v3')
  where
    scalarMul :: Double -> Vec -> Vec
    scalarMul d (R2 a b) = R2 (a * d) (b * d)
    v1' = r2 . (fromIntegral *** fromIntegral) $ v1
    v2' = r2 . (fromIntegral *** fromIntegral) $ v2
    v3' = r2 . (fromIntegral *** fromIntegral) $ v3
    r'  = fromIntegral r

