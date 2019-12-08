-- Copyright 2019, Jack Stanek. All Rights Reserved.

module Parser (parseSexp) where

import Control.Applicative

import Ast

newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

instance Functor Parser where
  fmap f (Parser p) = Parser $ \input -> do
    (result, rest) <- p input
    Just (f result, rest)

instance Applicative Parser where
  pure c = Parser $ \input -> Just (c, input)
  (Parser lhs) <*> (Parser rhs) = Parser $ \input -> do
    (f, lrest) <- lhs input
    (rresult, rrest) <- rhs lrest
    Just (f rresult, rrest)

instance Alternative Parser where
  empty = Parser $ \_ -> Nothing
  (Parser lhs) <|> (Parser rhs) = Parser $ \input ->
                                             lhs input <|> rhs input

charP :: Char -> Parser Char
charP chr =
  Parser $ \input ->
  case input of
    "" -> Nothing
    (x:xs) -> if x == chr then Just (chr, xs) else Nothing

stringP :: String -> Parser String
stringP = sequenceA . map charP

-- concrete parser
type SexpParser = Parser Sexp

nilP :: SexpParser
nilP = (\_ -> Nil) <$> stringP "nil"

boolP :: SexpParser
boolP = f <$> (stringP "true" <|> stringP "false")
  where f "true" = BoolVal True
        f "false" = BoolVal False
        f _ = error "invalid boolean literal"

-- symbolP :: SexpParser
-- symbolP = (\s -> Value $ SymbolVal s) <$> stringP

parseSexp :: String -> Maybe Sexp
parseSexp input = case (runParser boolP input) of
                    Just (sexp, "") -> Just sexp
                    _ -> Nothing
