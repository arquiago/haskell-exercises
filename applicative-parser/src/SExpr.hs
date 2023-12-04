{-# LANGUAGE GADTs #-}
module SExpr where

import AParser
import Data.Char
import Control.Applicative

type Ident = String

data Atom where
  N :: Integer -> Atom
  I :: Ident   -> Atom
  deriving (Eq, Show)

data SExpr where
  A    :: Atom    -> SExpr
  Comb :: [SExpr] -> SExpr
  deriving (Eq, Show)

-- Item 6

zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = (:) <$> p <*> zeroOrMore p <|> pure []

oneOrMore :: Parser a -> Parser [a]
oneOrMore p = (:) <$> p <*> zeroOrMore p 

-- Item 7

spaces :: Parser String
spaces = zeroOrMore $ satisfy isSpace

ident :: Parser String
ident = (:) <$> satisfy isAlpha <*> zeroOrMore (satisfy isAlphaNum)
-- Item 8

parseSExpr :: Parser SExpr
parseSExpr = spaces *> (parseAtom <|> parseComb) <* spaces
  where
    parseAtom = A <$> parseAtom'
    parseAtom' = N <$> posInt <|> I <$> ident

    parseComb = Comb <$> (char '(' *> oneOrMore parseSExpr <* char ')')

