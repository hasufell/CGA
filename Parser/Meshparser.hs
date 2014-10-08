{-# OPTIONS_HADDOCK ignore-exports #-}

module Parser.Meshparser (meshToArr) where

import Control.Applicative
import Diagrams.TwoD.Types
import LinearAlgebra.Vector
import Parser.Core


-- | Convert a text String with multiple vertices into
-- an array of float tuples.
meshToArr :: String   -- ^ the string to convert
          -> [PT]     -- ^ the resulting vertice table
meshToArr xs = fmap (p2)                       .
                 fmap (\(Just (x, _)) -> x)    .
                 filter (/= Nothing)           .
                 fmap (runParser parseVertice) .
                 lines                         $
                 xs


-- | Creates a Parser that accepts a single vertice, such as 'v 1.0 2.0'.
parseVertice :: Parser (Double, Double)
parseVertice = (,)                                 <$>
                 (char 'v' *> spaces *> posDouble) <*>
                 (spaces *> posDouble)
