-- Copyright 2019, Jack Stanek. All Rights Reserved.

module Parser where

type LinePos = (Int, Int)
type ParseError = (String, LinePos)
type ParseInput = [(Char, Int)]
newtype Parser a = Parser { runParser :: String -> Either ParseError (String, a) }

linePos :: Int -> String -> Maybe LinePos
linePos pos input = findLinePos pos input 1 1
  where findLinePos pos input line col =
          case input of
            ""        -> Nothing
            ('\n':xs) -> findLinePos (pos - 1) xs (line + 1) 1
            (x:xs)    -> if pos == 0 then Just (line, col)
                         else findLinePos (pos - 1) xs line (col + 1)

-- charP :: Char -> Parser Char
-- charP c =
--   Parser $ \input ->
--   case input of
--     "" -> Left ("Reached end of input expecting '" ++ show c ++ "'")
--     (x:xs) -> _b
