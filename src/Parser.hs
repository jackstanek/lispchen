-- Copyright 2019, Jack Stanek. All Rights Reserved.

module Parser (parseSexp) where

import Control.Applicative
import Data.Maybe (maybeToList)

import Ast

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
numberP = lexeme $ IntVal . read <$> n
  where digits  = some $ oneOfP ['0'..'9']
        neg     = maybeToList <$> (optionP $ charP '-')
        n       = Parser $ \input -> do
          (sign, input) <- runParser neg input
          (digits, input) <- runParser digits input
          Right (sign ++ digits, input)

atomP = nilP <|> boolP <|> symvalP <|> numberP <|> quotedP

consP :: SexpParser
consP = lparenP *>
        (Cons <$> (many sexpP)) <*
        rparenP

quotedP :: SexpParser
quotedP = lexeme $ (charP '\'') *> (Quoted <$> sexpP)

keywordP = lexeme . stringP

ifP :: SexpParser
ifP = lparenP *> (keywordP "if") *> ifParser <* rparenP
  where ifParser = Parser $ \input -> do
          (condition, input) <- runParser sexpP input
          (then', input) <- runParser sexpP input
          (else', input) <- runParser sexpP input
          Right (If condition then' else', input)

letP :: SexpParser
letP =
  lparenP *> (keywordP "let") *> letParser <* rparenP
  where letParser = Parser $ \input -> do
          (_, input) <- runParser lparenP input
          (bindings, input) <- runParser (some binding) input
          (_, input) <- runParser rparenP input
          (body, input) <- runParser sexpP input
          (if length bindings < 1
            then Left "not enough bindings in let expression"
            else Right (Let bindings body, input))
        binding = lparenP *>
          (Parser $ \input -> do
              (sym, input) <- runParser symbolP input
              (val, input) <- runParser sexpP input
              Right ((sym, val), input)) <* rparenP

lambdaP :: SexpParser
lambdaP =
  lparenP *> (keywordP "lambda") *> lambdaParser <* rparenP
  where lambdaParser = Parser $ \input -> do
          (_, input) <- runParser lparenP input
          (args, input) <- runParser (many $ symbolP) input
          (_, input) <- runParser rparenP input
          (body, input) <- runParser sexpP input
          Right (Lambda args body, input)

sexpP :: SexpParser
sexpP = ifP <|> letP <|> lambdaP <|> atomP <|> consP

parseSexp :: String -> Either String Sexp
parseSexp input = case (runParser sexpP input) of
                    Right (sexp, "") -> Right sexp
                    Right (_, rest) -> Left $ "stopped parsing: " ++ rest
                    Left l -> Left l
