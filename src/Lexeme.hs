module Lexeme(Lexeme(Lexeme)) where

import Token

-- A lexeme is a Token plus the string that makes the token
data Lexeme = Lexeme { token :: Token, tokenStr :: String } deriving(Show)

instance Eq Lexeme where
  x == y = token x == token y

