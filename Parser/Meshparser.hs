{-# OPTIONS_HADDOCK ignore-exports #-}

module Parser.Meshparser (meshToArr, facesToArr) where

import Algebra.VectorTypes
import Control.Applicative
import Data.Maybe
import Diagrams.TwoD.Types
import Parser.Core


-- |Convert a text String with multiple vertices and faces into
-- a list of vertices, ordered by the faces specification.
facesToArr :: String -> [[PT]]
facesToArr str = fmap (fmap (\y -> meshs str !! (fromIntegral y - 1)))
                      (faces str)
  where
    meshs = meshToArr
    faces = fmap fst . catMaybes . fmap (runParser parseFace) . lines


-- |Convert a text String with multiple vertices into
-- an array of float tuples.
meshToArr :: String   -- ^ the string to convert
          -> [PT]     -- ^ the resulting vertice table
meshToArr =
  fmap (p2 . fst)
    . catMaybes
    . fmap (runParser parseVertice)
    . lines


-- |Creates a Parser that accepts a single vertice, such as 'v 1.0 2.0'.
parseVertice :: Parser (Double, Double)
parseVertice =
  (,)
    <$> (char 'v' *> spaces *> allDouble)
    <*> (spaces *> allDouble)


parseFace :: Parser [Integer]
parseFace = char 'f' *> oneOrMore (spaces *> posInt)
