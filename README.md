# haskell-lua
haskell-lua is a reimplementation of a project I did in college to implement a basic lua interpreter. This project was intended to help learn principles of programming languages as well as act as an exercise in learning a new language. A interpreter is a non-trivial application and requires a more indepth knowledge of the language used to implement the interpreter.

Some simplifications to lua are being made for the purpose of this interpreter.
1. Polish notation will be used for ease of parsing.
2. All tokens must be separated by a space. For example a print statement will be written as `print ( + 100 8 )`

## Grammar
The grammar to be used for this interpreter is as follows

```
<block> -> <statement> | <statement> <block>

<statement> -> <if_statement> 
    | <assignment_statement>
    | <while_statement> 
    | <print_statement> 
    | <repeat_statement>

<if_statement> -> if <boolean_expression> then <block> else <block> end

<boolean_expression> -> <relative_op> <arithmetic_expression> <arithmetic_expression>

<while_statement> -> while <boolean_expression> do <block> end

<assignment_statement> -> id <assignment_operator> <arithmetic_expression>

<print_statement> -> print ( <arithmetic_expression> )

<repeat_statement> -> repeat <block> until <boolean_expression>

<arithmetic_expression> -> <id> 
    | <literal_integar>
    | <arithmetic_op> <arithmetic_expression> <arithmetic_expression>
```
