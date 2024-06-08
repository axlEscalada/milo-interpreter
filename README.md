# milo-interpreter

Milo interpreter, language written in Zig following crafting interpreters book.

## Scanning

Also known as lexical analysis or lexing.
The scanner take int the linear stream of characters and chunks them together into a series of tokens.
Tokens could be single characters like ( or several characters long like numbers, string literals or identifiers.

## Parsing

A parser takes the tokens and build a tree structure that mirrors the nested nature of the grammar.
These tree have a couple of different names, parse tree or abstract syntax tree (AST).

## Static Analysis

Here is where we find out where variables are declared, its scope and we can type check at this point
for statically typed languages.
Then we can store all this analysis in the syntax tree, in a lookup table or transforming it in a new data structure.
This is the front end.

## Intermediate representation

Intermediate representation (IR) acts an interface between the source code and the destination forms.
This let you support multiple source languages and target platforms with less effort.
For example we can write one front end for each source that produces the IR, then one back end for each target architecure.
| Lang | IR | Arch   |
|------|----|--------|
| Java |    | x86    |
| C    | IR | ARM    |
| Go   |    | x86_64 |

## Optimization

After understand the code we can optimize it. A simple example is constant folding where some expression
always evaluate to the same value, we can do the evaluation at compile time and replace the code for the expression with its result;

`pennyArea = 3.14159 * (0.75 / 2) * (0.75 / 2);`

Calculate it in the compiler and change the code to:
`pennyArea = 0.4417860938;`
