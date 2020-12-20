# CompilerRAE

## About 

This is a small project for Programming Language Design 2020, NCU.  
  
Lexer, Parser and Evaluater for Rational Numbers

## Problem Disscription - Rational Arithmetic Evaluation

In this assignment, you are asked to implement a arithmetic evaluator with rational numbers in Haskell.

Minimal requirements:

- Able to do add/subtract/multiply/divide computations on rational numbers.
- Print the result in simplified form
- Store rational number as two integers(numerator and denominator) instead of float numbers.

Example:  

- `(1%2) + (3%4) * (5%6) - (2%1) / (1%2)` will evaluates to `(-23) % 8`
- `(1%1) / (2%0)` will report a “divide by zero” error

## Overview of the toyLexerRAE

The code is to find the difference between implmenting a simple Compiler in Haskell and C, which one is a functional programming language and another is imperative.

- For Haskell part, I reference this [link](https://www.schoolofhaskell.com/school/starting-with-haskell/basics-of-haskell) for basic implementation. 
- For C part, you can check out my other repositoris. 
- For Rust, checkout [sheccr](https://github.com/yppan/sheccr)

## Data structure I defined 

Define in Lexer.hs:
``` Haskell 
data Operator = Add | Sub | Mul | Div | SPLIT
    deriving (Show, Eq)

data Token = TokOp Operator
           | TokNum Int
           | TokLParen 
           | TokRParen
           | TokSep     -- %
           | TokEnd
    deriving (Show, Eq)

operator :: Char -> Operator
operator c | c == '+' = Add
           | c == '-' = Sub
           | c == '*' = Mul
           | c == '/' = Div
```

Define in Parser.hs:
```Haskell
data Tree = SumNode Operator Tree Tree
          | ProdNode Operator Tree Tree
          | RatNode Int Int   -- node to save ratioal number
    deriving Show
```
## Idioms 

Simple Top-Down Parsing

## How to run code 

1. Clone to local:        
   `$ git clone https://github.com/yppan/toyLexerRAE/`    
2. `$ cd toyLexerRAE`
3. `$ stack build`
4. `$ stack run`
