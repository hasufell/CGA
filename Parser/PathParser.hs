{-# OPTIONS_HADDOCK ignore-exports #-}

module Parser.PathParser where

import Control.Applicative
import Parser.Core
import Algorithms.RangeSearch.QuadTree (Quad(NW, NE, SW, SE), Orient(North, South, West, East))


-- |Parse a string such as "ne, n, sw, e" into
-- [Quad NE, Orient North, Quad SW, Orient East].
stringToQuads :: String -> [Either Quad Orient]
stringToQuads str = case runParser parsePath str of
  Nothing -> []
  Just xs -> fst xs
  where
    parsePath = zeroOrMore ((parseQuad <|> parseOrient)
                             <* zeroOrMore (char ',')
                             <* spaces)


-- |Parses a string that represents a single squad into the
-- QuadOrOrient ADT.
parseQuad :: Parser (Either Quad Orient)
parseQuad =
  const (Left NW) <$> (string "nw" <|> string "NW")
    <|> const (Left NE) <$> (string "ne" <|> string "NE")
    <|> const (Left SW) <$> (string "sw" <|> string "SW")
    <|> const (Left SE) <$> (string "se" <|> string "SE")


-- |Parses a string that represents a single Orientation into the
-- QuadOrOrient ADT.
parseOrient :: Parser (Either Quad Orient)
parseOrient =
  const (Right North) <$> (string "n" <|> string "N")
    <|> const (Right South) <$> (string "s" <|> string "S")
    <|> const (Right West) <$> (string "w" <|> string "W")
    <|> const (Right East) <$> (string "e" <|> string "E")
