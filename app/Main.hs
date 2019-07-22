module Main where

import Lexer

main :: IO ()
main = do 
    let program = "function ( )\n\
    \    print ( 100 )\n\
    \ end"
    let lexemes = lex_check program
    print "test"

