module Main where

import Data.Char

data Operator = Add | Sub | Mul | Div | SPLIT
    deriving (Show, Eq)

data Token = TokOp Operator
           | TokNum Int
           | TokLParen 
           | TokRParen
           | TokSep     -- %
    deriving (Show, Eq)

operator :: Char -> Operator
operator c | c == '+' = Add
           | c == '-' = Sub
           | c == '*' = Mul
           | c == '/' = Div

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


main :: IO ()
main = do putStrLn "             ^  -  ^\n\
                   \            ( .   . )\n\
                   \              =>;<=\n\
                   \             /     \\\n\
                   \            |       |\n\
                   \ * Rational Arithmetic Evaluation *\n\
                   \   Example: (1%2) + (3%4) * (5%6) "
          inputString <- getLine
          putStrLn ("Your input is \"" ++ inputString ++ "\"!")
