{-# OPTIONS_HADDOCK ignore-exports #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Test.MyPrelude where

import MyPrelude
import Test.QuickCheck


splitByProp1 :: Positive Int
             -> NonEmptyList (Positive Int)
             -> NonEmptyList (Positive Int)
             -> Bool
splitByProp1 (Positive x) (NonEmpty xs) (NonEmpty ys) =
  splitBy (== negate x) (xs' ++ [negate x] ++ ys') == [xs', ys']
  where
    xs' = fmap getPositive xs
    ys' = fmap getPositive ys


splitByProp2 :: Positive Int
             -> NonEmptyList (Positive Int)
             -> NonEmptyList (Positive Int)
             -> Bool
splitByProp2 (Positive x) (NonEmpty xs) (NonEmpty ys) =
  splitBy (== negate x) (xs' ++ [negate x] ++ ys' ++ [negate x] ++ xs')
  ==
  [xs', ys', xs']
  where
    xs' = fmap getPositive xs
    ys' = fmap getPositive ys


-- splitting by an element that is not in the list should leave the list
-- untouched
splitByProp3 :: Positive Int
             -> NonEmptyList (Positive Int)
             -> Bool
splitByProp3 (Positive x) (NonEmpty xs) =
  splitBy (== negate x) xs' == [xs']
  where
    xs' = fmap getPositive xs

