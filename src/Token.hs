module Token
(
    Token
    (
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
    )
) where

data Token
    = ID     -- begins with character a-z then alpha numeric.
    | INT    -- number belonging to the integers.
    | ASSIGN -- =
    | LE     -- <= 
    | LT     -- < 
    | GE     -- >=
    | GT     -- > 
    | EQ     -- ==
    | NE     -- ~=
    | ADD    -- +
    | SUB    -- -
    | DIV    -- /
    | MUL    -- *
    | IF     -- if
    | WHILE  -- while
    | DO     -- do
    | LPAR   -- (
    | RPAR   -- )
    | REPEAT -- repeat
    | UNTIL  -- until
    | THEN   -- then
    | FUN    -- function
    | END    -- end
    | ELSE   -- else
    | PRINT  -- print
    deriving(Eq, Show)


