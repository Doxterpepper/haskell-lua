-- Lexical analysis module. Includes lexeme data types

module Lexer
(
    lexCheck
) where

import Token
import Lexeme

isNumeric :: Char -> Bool
isNumeric c = c >= '0' && c <= '9'

isValidIdChar :: Char -> Bool
isValidIdChar c = c >= 'a' && c <= 'z'
    || c >= 'A' && c <= 'Z'
    || c >= '0' && c <= '9'

-- Check if a string can be parsed as an integer or not.
isInteger :: String -> Bool
isInteger "" = False
isInteger ('-':xs) = case dropWhile isNumeric xs of
    "" -> True
    _ -> False
isInteger s = case dropWhile isNumeric s of
    "" -> True
    _ -> False

-- Check if a string can be interpreted as an id.
validId :: String -> Bool
validId "" = False
validId (x:xs) | x >= 'a' && x <= 'z' || x >= 'A' && x <= 'Z' =
    case dropWhile isValidIdChar xs of
    "" -> True
    _ -> False


-- Build a list of lexemes from a list of strings. Strings are assumed to be cleaned
-- of whitespace.
buildLexemes :: [String] -> [Lexeme]
buildLexemes [] = []
buildLexemes (x:xs) | x == "+" = Lexeme ADD x : buildLexemes xs
buildLexemes (x:xs) | x == "=" = Lexeme ASSIGN x : buildLexemes xs
buildLexemes (x:xs) | x == "<=" = Lexeme LE x : buildLexemes xs
buildLexemes (x:xs) | x == "<" = Lexeme Token.LT x : buildLexemes xs
buildLexemes (x:xs) | x == ">=" = Lexeme GE x : buildLexemes xs
buildLexemes (x:xs) | x == ">" = Lexeme Token.GT x : buildLexemes xs
buildLexemes (x:xs) | x == "~=" = Lexeme NE x : buildLexemes xs
buildLexemes (x:xs) | x == "-" = Lexeme SUB x : buildLexemes xs
buildLexemes (x:xs) | x == "/" = Lexeme DIV x : buildLexemes xs
buildLexemes (x:xs) | x == "*" = Lexeme MUL x : buildLexemes xs
buildLexemes (x:xs) | x == "if" = Lexeme IF x : buildLexemes xs
buildLexemes (x:xs) | x == "while" = Lexeme WHILE x : buildLexemes xs
buildLexemes (x:xs) | x == "do" = Lexeme DO x : buildLexemes xs
buildLexemes (x:xs) | x == "(" = Lexeme LPAR x : buildLexemes xs
buildLexemes (x:xs) | x == ")" = Lexeme RPAR x : buildLexemes xs
buildLexemes (x:xs) | x == "repeat" = Lexeme REPEAT x : buildLexemes xs
buildLexemes (x:xs) | x == "until" = Lexeme UNTIL x : buildLexemes xs
buildLexemes (x:xs) | x == "then" = Lexeme THEN x : buildLexemes xs
buildLexemes (x:xs) | x == "function" = Lexeme FUN x : buildLexemes xs
buildLexemes (x:xs) | x == "end" = Lexeme END x : buildLexemes xs
buildLexemes (x:xs) | x == "else" = Lexeme ELSE x : buildLexemes xs
buildLexemes (x:xs) | x == "print" = Lexeme PRINT x : buildLexemes xs
buildLexemes (x:xs) | x!!0 == '-' || isInteger x = Lexeme INT x : buildLexemes xs
buildLexemes (x:xs) | (x!!0 >= 'a' && x!!0 <= 'z' || x!!0 >= 'A' && x!!0 <= 'Z') && validId x = Lexeme ID x : buildLexemes xs
buildLexemes (x:xs) | x /= "" = error ("Unknown token: " ++ x)

-- Do the lexical analysis.
lexCheck :: String -> [Lexeme]
lexCheck source = buildLexemes (words source)
