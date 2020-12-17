module Main where

import Data.Char

data Operator = ADD | SUB | MUL | DIV | SPLIT
    deriving (Show, Eq)

data Parentesis = LBR | RBR
    deriving (Show, Eq)

data Token = TokOp Operator
           | TokNum Int
           | TokBR Parentesis
    deriving (Show, Eq)

operator :: Char -> Operator
operator c | c == '+' = ADD
           | c == '-' = SUB
           | c == '*' = MUL
           | c == '/' = DIV
           | c == '%' = SPLIT

parentesis :: Char -> Parentesis 
parentesis c | c == '(' = LBR
             | c == ')' = RBR

tokenize :: String -> [Token]
tokenize [] = []
tokenize (c : cs) 
    | elem c "+-*/%" = TokOp (operator c) : tokenize cs
    | elem c "()" = TokBR (parentesis c) : tokenize cs   
    | isDigit c  = TokNum (digitToInt c) : tokenize cs
    | isSpace c  = tokenize cs
    | otherwise  = error $ "Cannot tokenize " ++ [c]


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
