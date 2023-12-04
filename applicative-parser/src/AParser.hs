{-# LANGUAGE GADTs #-}
module AParser where

import Data.Char
import Control.Applicative

newtype Parser a where
  P :: (String -> Maybe (a, String)) -> Parser a

runParser :: Parser a -> String -> Maybe (a, String)
runParser (P f) = f

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = P f
  where
    f [] = Nothing  
    f (x:xs)        
                    
                    
        | p x       = Just (x, xs)
        | otherwise = Nothing

char :: Char -> Parser Char
char c = satisfy (== c)

posInt :: Parser Integer
posInt = P f
  where
    f xs
      | null ns   = Nothing
      | otherwise = Just (read ns, rest)
      where (ns, rest) = span isDigit xs

-- Item 1

instance Functor Parser where
    fmap f (P eta) = P $ \input -> 
        case eta input of
             Just (x, res) -> Just (f x, res)
             Nothing       -> Nothing 

-- Item 2

instance Applicative Parser where
  pure a = P $ \inp -> Just (a, inp)
  (<*>) (P fh) (P fa) = P $ \inp -> 
      case fh inp of
           Just (f, resto) -> 
               case fa resto of
                    Just (v, resto') -> Just (f v, resto')
                    Nothing          -> Nothing 
           Nothing -> Nothing 

-- Item 3

abParser :: Parser (Char, Char)
abParser =  (,) <$> char 'a' <*> char 'b'

takeAndDisc :: Char -> Char -> () 
takeAndDisc _ _  = () 

abParser_ :: Parser ()
abParser_ = takeAndDisc <$> char 'a' <*> char 'b'

intPair :: Parser [Integer]
intPair = (\x _ y -> [x, y]) <$> posInt <*> char ' ' <*> posInt
  
-- Item 4

instance Alternative Parser where
  empty = P (const Nothing)
  (<|>) (P f) (P g) = P $ \inp -> 
    case f inp of 
         Nothing -> g inp 
         v -> v 
    
-- Item 5

intOrUpperCase :: Parser ()
intOrUpperCase = const () <$> posInt <|> const () <$> satisfy isUpper
