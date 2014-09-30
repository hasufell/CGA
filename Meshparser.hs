module Meshparser where

import Control.Applicative
import Parser

-- | Convert a text String with multiple vertices into
-- an array of float tuples.
meshToArr :: String           -- ^ the string to convert
          -> [(Float, Float)] -- ^ the resulting float tuple
meshToArr xs = fmap (\(Just (x, _)) -> x)      .
                 filter (/= Nothing)           .
                 fmap (runParser parseVertice) .
                 lines                         $
                 xs

-- | Creates a Parser that accepts a single vertice, such as 'v 1.0 2.0'.
parseVertice :: Parser (Float, Float)
parseVertice = liftA2 (,)
                 (char 'v' *> spaces *> posFloat)
                 (spaces *> posFloat)
