{-# OPTIONS_HADDOCK ignore-exports #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Test.Vector where

import Algebra.Vector
import Control.Applicative
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
  = getAngle (R2 x2 0) (R2 0 y1) == pi / 2
