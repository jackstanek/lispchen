-- Copyright 2019, Jack Stanek. All Rights Reserved.

module Parser (parseSexp) where

import Control.Applicative
import Data.Maybe (maybeToList)

import ConcreteSyntax

newtype Parser a = Parser { runParser :: String -> Either String (a, String) }

instance Functor Parser where
  fmap f (Parser p) = Parser $ \input -> do
    (result, rest) <- p input
    Right (f result, rest)

instance Applicative Parser where
  pure c = Parser $ \input -> Right (c, input)
  (Parser lhs) <*> (Parser rhs) = Parser $ \input -> do
    (f, lrest) <- lhs input
    (rresult, rrest) <- rhs lrest
    Right (f rresult, rrest)

instance Alternative Parser where
  empty = Parser $ \_ -> Left "empty parser"
  (Parser lhs) <|> (Parser rhs) = Parser $ \input ->
    let l = lhs input in
      case l of
        Right _ -> l
        Left _ -> rhs input

charP :: Char -> Parser Char
charP chr = Parser $ \input ->
  case input of
    (x:xs) -> if x == chr
              then Right (chr, xs)
              else Left $ "unexpected character " ++ show x
    "" -> Left "reached end of input"

letterP :: Parser Char
letterP = oneOfP $ ['A'..'Z'] ++ ['a'..'z']

stringP :: String -> Parser String
stringP = sequenceA . map charP

-- combinators

oneOfP :: [Char] -> Parser Char
oneOfP "" = empty
oneOfP (x:xs) = (charP x) <|> (oneOfP xs)

exceptP :: [Char] -> Parser Char
exceptP s = Parser $ \input ->
  case input of
    "" -> Left "reached end of input"
    (x:xs) ->
      if not $ elem x s then
        Right (x, xs)
      else Left $ "unexpected character " ++ show x

optionP :: Parser a -> Parser (Maybe a)
optionP (Parser p) = Parser $ \input ->
  let result = p input in
    Right $ case result of
              Left _ -> (Nothing, input)
              Right (v, r) -> (Just v, r)

-- concrete parser
type SexpParser = Parser Sexp

lexeme :: Parser a -> Parser a
lexeme p = whitespaceP *> p
  where whitespaceP = many $ oneOfP " \n\t"

lparenP = lexeme $ charP '('
rparenP = lexeme $ charP ')'

nilP = lexeme $ (\_ -> Nil) <$> (stringP "nil" <|> stringP "()")

symvalP = SymbolVal <$> symbolP
  where symbolP = lexeme $ Symbol <$> some letterP

numberP = lexeme $ IntVal . read <$> n
  where digits  = some $ oneOfP ['0'..'9']
        neg     = maybeToList <$> (optionP $ charP '-')
        n       = Parser $ \input -> do
          (sign, input) <- runParser neg input
          (digits, input) <- runParser digits input
          Right (sign ++ digits, input)

stringLitP = StringVal <$> (doublequoteP *> strContentP <* doublequoteP)
  where dq = '"'
        doublequoteP = charP dq
        strContentP = many $ exceptP [dq]

quotedP :: SexpParser
quotedP = lexeme $ (charP '\'') *> (Quoted <$> sexpP)

atomP = nilP <|> symvalP <|> numberP <|> stringLitP <|> quotedP

consP :: SexpParser
consP = lparenP *>
        (Cons <$> (many sexpP)) <*
        rparenP

sexpP :: SexpParser
sexpP = atomP <|> consP

parseSexp :: String -> Either String Sexp
parseSexp s = runParser sexpP s >>= \(result, remaining) ->
    if null remaining then
      Right result
    else Left "error parsing; excess characters"
