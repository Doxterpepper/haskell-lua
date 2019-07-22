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
    lex_check
) where

-- Lexeme data type
data Lexeme = ID String -- begins with character a-z then alpha numeric
    | INT Integer       -- number belonging to the integers
    | ASSIGN            -- =
    | LE                -- <= 
    | LT                -- < 
    | GE                -- >=
    | GT                -- > 
    | EQ                -- ==
    | NE                -- ~=
    | ADD               -- +
    | SUB               -- -
    | DIV               -- /
    | MUL               -- *
    | IF                -- if
    | WHILE             -- while
    | DO                -- do
    | LPAR              -- (
    | RPAR              -- )
    | REPEAT            -- repeat
    | UNTIL             -- until
    | THEN              -- then
    | FUN               -- function
    | END               -- end
    | ELSE              -- else
    | PRINT             -- print

-- check if a string can be parsed as an integer or not
is_natural :: String -> Bool
is_natural (x:xs) = (x >= '0' || x <= '9') && is_natural xs

-- check if a string can be interpreted as an id
is_id :: String -> Bool
is_id (x:xs) = (x >= 'a' && x <= 'z' || x >= 'A' && x <= 'Z' || x >= '0' && x <= '9') && is_id xs

-- Build a list of lexemes from a list of strings. Strings are assumed to be cleaned
-- of whitespace
build_lexemes :: [String] -> [Lexeme]
build_lexemes (x:xs) | x == "+" = ADD : build_lexemes xs
build_lexemes (x:xs) | x == "=" = ASSIGN : build_lexemes xs
build_lexemes (x:xs) | x == "<=" = LE : build_lexemes xs
build_lexemes (x:xs) | x == "<" = Lexer.LT : build_lexemes xs
build_lexemes (x:xs) | x == ">=" = GE : build_lexemes xs
build_lexemes (x:xs) | x == ">" = Lexer.GT : build_lexemes xs
build_lexemes (x:xs) | x == "~=" = NE : build_lexemes xs
build_lexemes (x:xs) | x == "-" = SUB : build_lexemes xs
build_lexemes (x:xs) | x == "/" = DIV : build_lexemes xs
build_lexemes (x:xs) | x == "*" = MUL : build_lexemes xs
build_lexemes (x:xs) | x == "if" = IF : build_lexemes xs
build_lexemes (x:xs) | x == "while" = WHILE : build_lexemes xs
build_lexemes (x:xs) | x == "do" = DO : build_lexemes xs
build_lexemes (x:xs) | x == "(" = LPAR : build_lexemes xs
build_lexemes (x:xs) | x == ")" = RPAR : build_lexemes xs
build_lexemes (x:xs) | x == "repeat" = REPEAT : build_lexemes xs
build_lexemes (x:xs) | x == "until" = UNTIL : build_lexemes xs
build_lexemes (x:xs) | x == "then" = THEN : build_lexemes xs
build_lexemes (x:xs) | x == "function" = FUN : build_lexemes xs
build_lexemes (x:xs) | x == "end" = END : build_lexemes xs
build_lexemes (x:xs) | x == "else" = ELSE : build_lexemes xs
build_lexemes (x:xs) | x == "print" = PRINT : build_lexemes xs
build_lexemes (x:xs) | x!!0 == '-' || is_natural x = INT(read x :: Integer) : build_lexemes xs
build_lexemes (x:xs) | (x!!0 >= 'a' && x!!0 <= 'z' || x!!0 >= 'A' && x!!0 <= 'Z') && is_id x = ID x : build_lexemes xs

-- Do the lexical analysis
lex_check :: String -> [Lexeme]
lex_check source = build_lexemes $ words source
