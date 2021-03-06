{-# OPTIONS_HADDOCK ignore-exports #-}

module Parser.Meshparser (meshToArr, facesToArr) where

import Control.Applicative
import Data.Attoparsec.ByteString.Char8
import Data.Either
import qualified Data.ByteString.Char8 as B
import Diagrams.TwoD.Types


-- |Convert a text String with multiple vertices and faces into
-- a list of vertices, ordered by the faces specification.
facesToArr :: B.ByteString -> [[P2 Double]]
facesToArr str = fmap (fmap (\y -> meshToArr str !! (fromIntegral y - 1)))
                      (faces str)
  where
    faces = rights . fmap (parseOnly parseFace) . B.lines


-- |Convert a text String with multiple vertices into
-- an array of float tuples.
meshToArr :: B.ByteString -- ^ the string to convert
          -> [P2 Double]         -- ^ the resulting vertice table
meshToArr =
  fmap p2
    . rights
    . fmap (parseOnly parseVertice)
    . B.lines


-- |Creates a Parser that accepts a single vertice, such as 'v 1.0 2.0'.
parseVertice :: Parser (Double, Double)
parseVertice =
  (,)
    <$> (char 'v' *> many' space *> double)
    <*> (many' space *> double)


parseFace :: Parser [Integer]
parseFace = char 'f' *> many1' (many' space *> decimal)
