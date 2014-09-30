module Parser (Parser,
               runParser,
               satisfy,
               char,
               posInt,
               posFloat,
               oneOrMore,
               zeroOrMore,
               spaces) where

import Control.Applicative
import Data.Char

-- |The parser type. It allows us to create specific parsers,
-- combine them and run them via 'runParser' to get a result.
newtype Parser a = MkParser { runParser :: String -> Maybe (a, String) }

-- |Functor instance.
instance Functor Parser where
  fmap f p = (inParser . fmap . fmap . first $ f) p

-- |Applicative functor instance.
instance Applicative Parser where
  pure a = MkParser (\s -> Just (a, s))
  (MkParser fp) <*> xp = MkParser $ \s ->
    case fp s of
      Nothing    -> Nothing
      Just (f,x) -> runParser (f <$> xp) x

-- |Alternative functor instance.
instance Alternative Parser where
  empty = MkParser (const Nothing)
  MkParser p1 <|> MkParser p2 = MkParser $ liftA2 (<|>) p1 p2

inParser :: ((String -> Maybe (a1, String))
         -> String
         -> Maybe (a, String))
         -> Parser a1
         -> Parser a
inParser f p = MkParser . f . runParser $ p

first :: (a -> b) -> (a,c) -> (b,c)
first f (x,y) = (f x, y)

-- |Creates a Parser that parses a Char depending on a given condition.
satisfy :: (Char -> Bool) -- ^ condition
        -> Parser Char    -- ^ created Parser
satisfy p = MkParser f
  where
    f [] = Nothing
    f (x:xs)
        | p x       = Just (x, xs)
        | otherwise = Nothing

-- |Creates a Parser that accepts a given Char.
char :: Char -> Parser Char
char c = satisfy (== c)

-- |Creates a Parser that accepts positive integers.
posInt :: Parser Integer
posInt = MkParser f
  where
    f xs
      | null ns   = Nothing
      | otherwise = Just (read ns, rest)
        where (ns, rest) = span isDigit xs

-- |Creates a Parser that accepts positive integers.
posFloat :: Parser Float
posFloat = read <$>
             liftA3 (\x y z -> x ++ [y] ++ z)
                    (MkParser f)
                    (char '.')
                    (MkParser f)
  where
    f xs
      | null ns   = Nothing
      | otherwise = Just (ns, rest)
        where (ns, rest) = span isDigit xs

-- |Convert a given Parser to a Parser that accepts zero or more occurences.
zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = oneOrMore p <|> pure []

-- |Convert a given Parser to a Parser that accepts one or more occurences.
oneOrMore :: Parser a -> Parser [a]
oneOrMore p = (:) <$> p <*> zeroOrMore p

-- |Creates a Parser that accepts spaces.
spaces :: Parser String
spaces = zeroOrMore (satisfy isSpace)
