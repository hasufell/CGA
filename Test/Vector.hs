{-# OPTIONS_HADDOCK ignore-exports #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Test.Vector where

import Algebra.Vector
import Control.Applicative
import Control.Arrow
{- import Control.Monad -}
import Diagrams.Coordinates
import Diagrams.Points
import Diagrams.TwoD.Types
import Test.QuickCheck


newtype RoundDouble = RoundDouble { getRD :: Double }
  deriving (Eq, Ord, Show, Read)


newtype PosRoundDouble = PosRoundDouble { getPRD :: Double }
  deriving (Eq, Ord, Show, Read)


newtype RoundR2 = RoundR2 { getRR2 :: R2 }
  deriving (Eq, Ord, Show, Read)


newtype PosRoundR2 = PosRoundR2 { getPRR2 :: R2 }
  deriving (Eq, Ord, Show, Read)


newtype RoundP2 = RoundP2 { getRP2 :: P2 }
  deriving (Eq, Ord, Show, Read)


newtype PosRoundP2 = PosRoundP2 { getPRP2 :: P2 }
  deriving (Eq, Ord, Show, Read)


instance Arbitrary RoundDouble where
  arbitrary = RoundDouble <$> fromIntegral <$> (arbitrary :: Gen Int)


instance Arbitrary PosRoundDouble where
  arbitrary = PosRoundDouble
                <$> fromIntegral
                -- (maxBound :: Int) instead of 100000 generates doubles
                <$> (choose (1, 10000) :: Gen Int)


instance Arbitrary RoundR2 where
  arbitrary = curry (RoundR2 . r2 . (getRD *** getRD))
                <$> (arbitrary :: Gen RoundDouble)
                <*> (arbitrary :: Gen RoundDouble)


instance Arbitrary PosRoundR2 where
  arbitrary = curry (PosRoundR2 . r2 . (getPRD *** getPRD))
                <$> (arbitrary :: Gen PosRoundDouble)
                <*> (arbitrary :: Gen PosRoundDouble)


instance Arbitrary RoundP2 where
  arbitrary = curry (RoundP2 . p2 . (getRD *** getRD))
                <$> (arbitrary :: Gen RoundDouble)
                <*> (arbitrary :: Gen RoundDouble)


instance Arbitrary PosRoundP2 where
  arbitrary = curry (PosRoundP2 . p2 . (getPRD *** getPRD))
                <$> (arbitrary :: Gen PosRoundDouble)
                <*> (arbitrary :: Gen PosRoundDouble)


instance Arbitrary R2 where
  arbitrary = curry r2 <$> arbitrary <*> arbitrary


instance Arbitrary P2 where
  arbitrary = curry p2 <$> arbitrary <*> arbitrary


-- the point describing the lower left corner of the square
-- must be part of the square
inRangeProp1 :: ((Double, Double), (Double, Double)) -> Bool
inRangeProp1 sq@((x1, y1), _) =
  inRange sq (p2 (x1, y1))


-- the point describing the upper right corner of the square
-- must be part of the square
inRangeProp2 :: ((Double, Double), (Double, Double)) -> Bool
inRangeProp2 sq@(_, (x2, y2)) =
  inRange sq (p2 (x2, y2))


-- the point describing the upper left corner of the square
-- must be part of the square
inRangeProp3 :: ((Double, Double), (Double, Double)) -> Bool
inRangeProp3 sq@((x1, _), (_, y2)) =
  inRange sq (p2 (x1, y2))


-- the point describing the lower right corner of the square
-- must be part of the square
inRangeProp4 :: ((Double, Double), (Double, Double)) -> Bool
inRangeProp4 sq@((_, y1), (x2, _)) =
  inRange sq (p2 (x2, y1))


-- generating random points within the square
inRangeProp5 :: ((Double, Double), (Double, Double)) -> Positive Double -> Positive Double -> Bool
inRangeProp5 sq@((x1, y1), (x2, y2)) (Positive a) (Positive b) =
  inRange sq (p2 (x1 + ((x2 - x1) / (a + 1)), y1 + ((y2 - y1) / (b + 1))))


-- generating random points outside of the square
inRangeProp6 :: ((Double, Double), (Double, Double)) -> Positive Double -> Positive Double -> Bool
inRangeProp6 sq@((x1, y1), (x2, y2)) (Positive a) (Positive b) =
  (not . inRange sq $ p2 (max x1 x2 + (a + 1), max y1 y2 + (b + 1)))
  && (not . inRange sq $ p2 (max x1 x2 + (a + 1), max y1 y2 - (b + 1)))
  && (not . inRange sq $ p2 (max x1 x2 - (a + 1), max y1 y2 + (b + 1)))
  && (not . inRange sq $ p2 (min x1 x2 - (a + 1), min y1 y2 - (b + 1)))
  && (not . inRange sq $ p2 (min x1 x2 + (a + 1), min y1 y2 - (b + 1)))
  && (not . inRange sq $ p2 (min x1 x2 - (a + 1), min y1 y2 + (b + 1)))


-- apply id function on the point
onPTProp1 :: P2 -> Bool
onPTProp1 pt = onPT id pt == pt


-- add a random value to the point coordinates
onPTProp2 :: P2 -> Positive R2 -> Bool
onPTProp2 pt (Positive (R2 rx ry))
  = onPT (\(x, y) -> (x + rx, y + ry)) pt /= pt


-- angle between two vectors both on the x-axis must be 0
getAngleProp1 :: Positive R2 -> Positive R2 -> Bool
getAngleProp1 (Positive (R2 x1 _)) (Positive (R2 x2 _))
  = getAngle (R2 x1 0) (R2 x2 0) == 0


-- angle between two vectors both on the y-axis must be 0
getAngleProp2 :: Positive R2 -> Positive R2 -> Bool
getAngleProp2 (Positive (R2 _ y1)) (Positive (R2 _ y2))
  = getAngle (R2 0 y1) (R2 0 y2) == 0


-- angle between two vectors both on the x-axis but with opposite direction
-- must be pi
getAngleProp3 :: Positive R2 -> Positive R2 -> Bool
getAngleProp3 (Positive (R2 x1 _)) (Positive (R2 x2 _))
  = getAngle (R2 (negate x1) 0) (R2 x2 0) == pi


-- angle between two vectors both on the y-axis but with opposite direction
-- must be pi
getAngleProp4 :: Positive R2 -> Positive R2 -> Bool
getAngleProp4 (Positive (R2 _ y1)) (Positive (R2 _ y2))
  = getAngle (R2 0 (negate y1)) (R2 0 y2) == pi


-- angle between vector in x-axis direction and y-axis direction must be
-- p/2
getAngleProp5 :: Positive R2 -> Positive R2 -> Bool
getAngleProp5 (Positive (R2 x1 _)) (Positive (R2 _ y2))
  = getAngle (R2 x1 0) (R2 0 y2) == pi / 2


-- commutative
getAngleProp6 :: Positive R2 -> Positive R2 -> Bool
getAngleProp6 (Positive v1) (Positive v2)
  = getAngle v1 v2 == getAngle v2 v1


-- Angle between two identical vectors. We can't check against 0
-- because of sqrt in vecLength.
getAngleProp7 :: PosRoundR2 -> Bool
getAngleProp7 (PosRoundR2 v)
  = getAngle v v < 0.0001 || isNaN (getAngle v v)


-- commutative
scalarProdProp1 :: R2 -> R2 -> Bool
scalarProdProp1 v1 v2 = v1 `scalarProd` v2 == v2 `scalarProd` v1


-- distributive, avoid doubles as we get messed up precision
scalarProdProp2 :: RoundR2 -> RoundR2 -> RoundR2 -> Bool
scalarProdProp2 (RoundR2 v1) (RoundR2 v2) (RoundR2 v3) =
  v1 `scalarProd` (v2 + v3)
  ==
  (v1 `scalarProd` v2) + (v1 `scalarProd` v3)


-- bilinear, avoid doubles as we get messed up precision
scalarProdProp3 :: RoundDouble -> RoundR2 -> RoundR2 -> RoundR2 -> Bool
scalarProdProp3 (RoundDouble r) (RoundR2 v1) (RoundR2 v2) (RoundR2 v3) =
  v1 `scalarProd` (scalarMul r v2 + v3)
  ==
  r * (v1 `scalarProd` v2) + (v1 `scalarProd` v3)


-- scalar multiplication
scalarProdProp4 :: RoundDouble -> RoundDouble -> RoundR2 -> RoundR2 -> Bool
scalarProdProp4 (RoundDouble s1) (RoundDouble s2) (RoundR2 v1) (RoundR2 v2)
  = scalarMul s1 v1 `scalarProd` scalarMul s2 v2
    ==
    s1 * s2 * (v1 `scalarProd` v2)


-- orthogonal
scalarProdProp5 :: Positive R2 -> Positive R2 -> Bool
scalarProdProp5 (Positive (R2 x1 _)) (Positive (R2 _ y2))
  = scalarProd (R2 x1 0) (R2 0 y2) == 0


-- this is almost the same as the function definition
dimToSquareProp1 :: (Double, Double) -> (Double, Double) -> Bool
dimToSquareProp1 (x1, x2) (y1, y2) =
  ((x1, y1), (x2, y2)) == dimToSquare (x1, x2) (y1, y2)


-- multiply scalar with result of vecLength or with the vector itself...
-- both results must be the same. We can't check against 0
-- because of sqrt in vecLength.
vecLengthProp1 :: PosRoundDouble -> R2 -> Bool
vecLengthProp1 (PosRoundDouble r) v
  = abs (vecLength v * r - vecLength (scalarMul r v)) < 0.0001


-- convert to vector and back again
pt2VecProp1 :: P2 -> Bool
pt2VecProp1 pt = (vec2Pt . pt2Vec $ pt) == pt


-- unbox coordinates and check if equal
pt2VecProp2 :: P2 -> Bool
pt2VecProp2 pt = (unr2 . pt2Vec $ pt) == unp2 pt


-- convert to point and back again
vec2PtProp1 :: R2 -> Bool
vec2PtProp1 v = (pt2Vec . vec2Pt $ v) == v


-- unbox coordinates and check if equal
vec2PtProp2 :: R2 -> Bool
vec2PtProp2 v = (unp2 . vec2Pt $ v) == unr2 v


-- vector from a to b must not be the same as b to a
vp2Prop1 :: P2 -> P2 -> Bool
vp2Prop1 p1' p2'
  | p1' == origin && p2' == origin = True
  | otherwise = vp2 p1' p2' /= vp2 p2' p1'


-- negating vector from a to be must be the same as vector b to a
vp2Prop2 :: P2 -> P2 -> Bool
vp2Prop2 p1' p2'
  | p1' == origin && p2' == origin = True
  | otherwise = vp2 p1' p2' == (\(R2 x y) -> negate x ^& negate y)
                                 (vp2 p2' p1')
                &&
                vp2 p2' p1' == (\(R2 x y) -> negate x ^& negate y)
                                 (vp2 p1' p2')


-- determinant of the 3 same points is always 0
detProp1 :: P2 -> Bool
detProp1 pt' = det pt' pt' pt' == 0
