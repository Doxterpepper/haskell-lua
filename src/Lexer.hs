-- Lexical analysis module. Includes lexeme data types
module Lexer
(
    Lexeme(
        ID,
        INT,
        ASSIGN,
        LE,
        LT,
        GE,
        GT,
        EQ,
        NE,
        ADD,
        SUB,
        DIV,
        MUL,
        IF,
        WHILE,
        DO,
        LPAR,
        RPAR,
        REPEAT,
        UNTIL,
        THEN,
        FUN,
        END,
        ELSE,
        PRINT
    ),
    lexCheck
) where

-- Lexeme data type.
data Lexeme
    = ID String   -- begins with character a-z then alpha numeric.
    | INT Integer -- number belonging to the integers.
    | ASSIGN      -- =
    | LE          -- <= 
    | LT          -- < 
    | GE          -- >=
    | GT          -- > 
    | EQ          -- ==
    | NE          -- ~=
    | ADD         -- +
    | SUB         -- -
    | DIV         -- /
    | MUL         -- *
    | IF          -- if
    | WHILE       -- while
    | DO          -- do
    | LPAR        -- (
    | RPAR        -- )
    | REPEAT      -- repeat
    | UNTIL       -- until
    | THEN        -- then
    | FUN         -- function
    | END         -- end
    | ELSE        -- else
    | PRINT       -- print

-- Check if a string can be parsed as an integer or not.
isNatural :: String -> Bool
isNatural (x:xs) = (x >= '0' || x <= '9') && isNatural xs

-- Check if a string can be interpreted as an id.
validId :: String -> Bool
validId (x:xs) = (x >= 'a' && x <= 'z' || x >= 'A' && x <= 'Z' || x >= '0' && x <= '9') && validId xs

-- Build a list of lexemes from a list of strings. Strings are assumed to be cleaned
-- of whitespace.
buildLexemes :: [String] -> [Lexeme]
buildLexemes (x:xs) | x == "+" = ADD : buildLexemes xs
buildLexemes (x:xs) | x == "=" = ASSIGN : buildLexemes xs
buildLexemes (x:xs) | x == "<=" = LE : buildLexemes xs
buildLexemes (x:xs) | x == "<" = Lexer.LT : buildLexemes xs
buildLexemes (x:xs) | x == ">=" = GE : buildLexemes xs
buildLexemes (x:xs) | x == ">" = Lexer.GT : buildLexemes xs
buildLexemes (x:xs) | x == "~=" = NE : buildLexemes xs
buildLexemes (x:xs) | x == "-" = SUB : buildLexemes xs
buildLexemes (x:xs) | x == "/" = DIV : buildLexemes xs
buildLexemes (x:xs) | x == "*" = MUL : buildLexemes xs
buildLexemes (x:xs) | x == "if" = IF : buildLexemes xs
buildLexemes (x:xs) | x == "while" = WHILE : buildLexemes xs
buildLexemes (x:xs) | x == "do" = DO : buildLexemes xs
buildLexemes (x:xs) | x == "(" = LPAR : buildLexemes xs
buildLexemes (x:xs) | x == ")" = RPAR : buildLexemes xs
buildLexemes (x:xs) | x == "repeat" = REPEAT : buildLexemes xs
buildLexemes (x:xs) | x == "until" = UNTIL : buildLexemes xs
buildLexemes (x:xs) | x == "then" = THEN : buildLexemes xs
buildLexemes (x:xs) | x == "function" = FUN : buildLexemes xs
buildLexemes (x:xs) | x == "end" = END : buildLexemes xs
buildLexemes (x:xs) | x == "else" = ELSE : buildLexemes xs
buildLexemes (x:xs) | x == "print" = PRINT : buildLexemes xs
buildLexemes (x:xs) | x!!0 == '-' || isNatural x = INT(read x :: Integer) : buildLexemes xs
buildLexemes (x:xs) | (x!!0 >= 'a' && x!!0 <= 'z' || x!!0 >= 'A' && x!!0 <= 'Z') && validId x = ID x : buildLexemes xs

-- Do the lexical analysis.
lexCheck :: String -> [Lexeme]
lexCheck source = buildLexemes $ words source
