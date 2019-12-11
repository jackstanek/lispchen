-- Copyright 2019, Jack Stanek. All Rights Reserved.

module Parser (parseSexp) where

import Control.Applicative

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
  (Parser lhs) <|> (Parser rhs) =
    Parser $ \input ->
               let l = lhs input in
                 case l of
                   Right _ -> l
                   Left _ -> rhs input

charP :: Char -> Parser Char
charP chr =
  Parser $ \input ->
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
          Right (If condition then' else', input)

letP :: SexpParser
letP =
  lparenP *> (lexeme $ stringP "let") *> letParser <* rparenP
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

sexpP :: SexpParser
sexpP = ifP <|> letP <|> atomP <|> consP

parseSexp :: String -> Either String Sexp
parseSexp input = case (runParser sexpP input) of
                    Right (sexp, "") -> Right sexp
                    Right (_, rest) -> Left $ "stopped parsing: " ++ rest
                    Left l -> Left l
