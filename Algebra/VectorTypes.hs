{-# OPTIONS_HADDOCK ignore-exports #-}

module Algebra.VectorTypes where

import Diagrams.TwoD.Types


type Vec     = R2
type PT      = P2
type Coord   = (Double, Double)
type Segment = (PT, PT)


data Alignment = CW
               | CCW
               | CL
  deriving (Eq)
