module Lexer where

import Data.Char ( isDigit, isSpace )
------  Define ------
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

------ Start Lexer  ------
tokenize :: String -> [Token]
tokenize [] = []
tokenize (c : cs) 
    | elem c "+-*/" = TokOp (operator c) : tokenize cs
    | c == '%'   = TokSep : tokenize cs
    | c == '('   = TokLParen : tokenize cs
    | c == ')'   = TokRParen : tokenize cs
    | isDigit c  = number c cs
    | isSpace c  = tokenize cs
    | otherwise  = error $ "Cannot tokenize " ++ [c]

number :: Char -> [Char] -> [Token]
number c cs = 
   let (digs, cs') = span isDigit cs in
   TokNum (read (c : digs)) : tokenize cs'

------ End Lexer ------