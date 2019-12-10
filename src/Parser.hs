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
  (Parser lhs) <|> (Parser rhs) =
    Parser $ \input -> lhs input <|> rhs input

charP :: Char -> Parser Char
charP chr =
  Parser $ \input ->
  case input of
    "" -> Nothing
    (x:xs) -> if x == chr then Just (chr, xs) else Nothing

letterP :: Parser Char
letterP = oneOfP $ ['A'..'Z'] ++ ['a'..'z']

stringP :: String -> Parser String
stringP = sequenceA . map charP

-- combinators

oneOfP :: String -> Parser Char
oneOfP "" = empty
oneOfP (x:xs) = (charP x) <|> (oneOfP xs)

-- concrete parser
type SexpParser = Parser Sexp

lexeme :: Parser a -> Parser a
lexeme p = whitespaceP *> p

whitespaceP :: Parser String
whitespaceP = many $ oneOfP " \n\t"

lparenP = lexeme $ charP '('
rparenP = lexeme $ charP ')'

nilP = lexeme $ (\_ -> Nil) <$> (stringP "nil" <|> stringP "()")

boolP = lexeme $ f <$> (stringP "#t" <|> stringP "#f")
  where f "#t" = BoolVal True
        f "#f" = BoolVal False
        f _ = error "invalid boolean literal"

symbolP = lexeme $ Symbol <$> some letterP
symvalP = SymbolVal <$> symbolP
numberP = lexeme $ IntVal . read <$> (some $ oneOfP ['0'..'9'])

atomP = nilP <|> boolP <|> symvalP <|> numberP <|> quotedP

consP :: SexpParser
consP = lparenP *>
        (Cons <$> (many sexpP)) <*
        rparenP

quotedP :: SexpParser
quotedP = lexeme $ (charP '\'') *> (Quoted <$> sexpP)

ifP :: SexpParser
ifP = lparenP *> (lexeme $ stringP "if") *> ifParser <* rparenP
  where ifParser = Parser $ \input -> do
          (condition, input) <- runParser sexpP input
          (then', input) <- runParser sexpP input
          (else', input) <- runParser sexpP input
          Just (If condition then' else', input)

letP :: SexpParser
letP =
  lparenP *> (lexeme $ stringP "let") *> letParser <* rparenP
  where letParser = Parser $ \input -> do
          (_, input) <- runParser lparenP input
          (bindings, input) <- runParser (some binding) input
          (_, input) <- runParser rparenP input
          (body, input) <- runParser sexpP input
          Just (Let bindings body, input)
        binding = lparenP *>
          (Parser $ \input -> do
              (sym, input) <- runParser symbolP input
              (val, input) <- runParser sexpP input
              Just ((sym, val), input)) <* rparenP

sexpP :: SexpParser
sexpP = ifP <|> letP <|> atomP <|> consP

parseSexp :: String -> Maybe Sexp
parseSexp input = case (runParser sexpP input) of
                    Just (sexp, "") -> Just sexp
                    _ -> Nothing
