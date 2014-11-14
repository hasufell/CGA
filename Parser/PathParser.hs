{-# OPTIONS_HADDOCK ignore-exports #-}

module Parser.PathParser where

import Control.Applicative
import Parser.Core
import Algorithms.RangeSearch.Core (Quad(NW, NE, SW, SE), Orient(North, South, West, East))


-- |Can either be an Orientation or a Quad, corresponding to the
-- Algorithms.RangeSearch module.
data QuadOrOrient = Orient Orient
                  | Quad Quad
  deriving (Show)


-- |Parse a string such as "ne, n, sw, e" into
-- [Quad NE, Orient North, Quad SW, Orient East].
stringToQuads :: String -> [QuadOrOrient]
stringToQuads str = case runParser parsePath str of
  Nothing -> []
  Just xs -> fst xs
  where
    parsePath = zeroOrMore ((parseQuad <|> parseOrient)
                             <* zeroOrMore (char ',')
                             <* spaces)


-- |Parses a string that represents a single squad into the
-- QuadOrOrient ADT.
parseQuad :: Parser QuadOrOrient
parseQuad =
  const (Quad NW) <$> (string "nw" <|> string "NW")
    <|> const (Quad NE) <$> (string "ne" <|> string "NE")
    <|> const (Quad SW) <$> (string "sw" <|> string "SW")
    <|> const (Quad SE) <$> (string "se" <|> string "SE")


-- |Parses a string that represents a single Orientation into the
-- QuadOrOrient ADT.
parseOrient :: Parser QuadOrOrient
parseOrient =
  const (Orient North) <$> (string "n" <|> string "N")
    <|> const (Orient South) <$> (string "s" <|> string "S")
    <|> const (Orient West) <$> (string "w" <|> string "W")
    <|> const (Orient East) <$> (string "e" <|> string "E")
