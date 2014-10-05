module Meshparser where

import Control.Applicative
import Parser

-- |The VTable is represented by a 'Double' tuple, 2-dimensional.
type VTable = [(Double, Double)]

-- | Convert a text String with multiple vertices into
-- an array of float tuples.
meshToArr :: String   -- ^ the string to convert
          -> VTable   -- ^ the resulting vertice table
meshToArr xs = fmap (\(Just (x, _)) -> x)      .
                 filter (/= Nothing)           .
                 fmap (runParser parseVertice) .
                 lines                         $
                 xs

-- | Creates a Parser that accepts a single vertice, such as 'v 1.0 2.0'.
parseVertice :: Parser (Double, Double)
parseVertice = (,)                                 <$>
                 (char 'v' *> spaces *> posDouble) <*>
                 (spaces *> posDouble)
