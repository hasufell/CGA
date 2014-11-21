{-# OPTIONS_HADDOCK ignore-exports #-}

module Parser.PathParser where

import Algorithms.QuadTree.QuadTree (Quad(NW, NE, SW, SE), Orient(North, South, West, East))
import Control.Applicative
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as B


-- |Parse a string such as "ne, n, sw, e" into
-- [Quad NE, Orient North, Quad SW, Orient East].
stringToQuads :: String -> [Either Quad Orient]
stringToQuads str = case parseOnly parsePath (B.pack str) of
  Left _   -> []
  Right xs -> xs
  where
    parsePath = many' ((parseQuad <|> parseOrient)
                             <* many' (char ',')
                             <* many' space)


-- |Parses a string that represents a single squad into the
-- QuadOrOrient ADT.
parseQuad :: Parser (Either Quad Orient)
parseQuad =
  const (Left NW) <$> (string (B.pack "nw") <|> string (B.pack "NW"))
    <|> const (Left NE) <$> (string (B.pack "ne") <|> string (B.pack "NE"))
    <|> const (Left SW) <$> (string (B.pack "sw") <|> string (B.pack "SW"))
    <|> const (Left SE) <$> (string (B.pack "se") <|> string (B.pack "SE"))


-- |Parses a string that represents a single Orientation into the
-- QuadOrOrient ADT.
parseOrient :: Parser (Either Quad Orient)
parseOrient =
  const (Right North) <$> (string (B.pack "n") <|> string (B.pack "N"))
    <|> const (Right South) <$> (string (B.pack "s") <|> string (B.pack "S"))
    <|> const (Right West) <$> (string (B.pack "w") <|> string (B.pack "W"))
    <|> const (Right East) <$> (string (B.pack "e") <|> string (B.pack "E"))
