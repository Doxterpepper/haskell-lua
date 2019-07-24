import Data.Typeable
import Control.Monad (unless)
import Test.HUnit
import Lexer

assertBool :: String -> Bool -> Assertion
assertBool msg b = unless b (assertFailure msg)

test1 = TestCase (assertEqual "Basic tokens are parse correctly"
    (lexCheck "+ = <= < >= > ~= - / * if while do ( ) repeat until\
    \ then function end else print") 
    ([
        Lexeme ADD "+",
        Lexeme ASSIGN "=",
        Lexeme LE "<=",
        Lexeme Lexer.LT "<",
        Lexeme GE ">=",
        Lexeme Lexer.GT ">",
        Lexeme NE "~=",
        Lexeme SUB "-",
        Lexeme DIV "/",
        Lexeme MUL "*",
        Lexeme IF "if",
        Lexeme WHILE "while",
        Lexeme DO "do",
        Lexeme LPAR "(",
        Lexeme RPAR ")",
        Lexeme REPEAT "repeat",
        Lexeme UNTIL "until",
        Lexeme THEN "then",
        Lexeme FUN "function",
        Lexeme END "end",
        Lexeme ELSE "else",
        Lexeme PRINT "print"
    ]))

test2 = TestCase (assertEqual "Parses valid IDs"
    (lexCheck "myId MyId MyId3")
    ([Lexeme ID "myId", Lexeme ID "MyId", Lexeme ID "MyId3"]))

test3 = TestCase (assertEqual "Parses valid integers"
    (lexCheck "-1234567890 1234567890")
    ([Lexeme INT "-1234567890", Lexeme INT "1234567890"]))

tests = TestList [TestLabel "test1" test1, TestLabel "test2" test2, TestLabel "test3" test3]

main = runTestTT tests
