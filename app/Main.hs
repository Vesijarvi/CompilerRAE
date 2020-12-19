module Main where

import Data.Char ( isDigit, isSpace )
import Lexer (tokenize)
import Parser (parse)
import Evaluator (evaluate, ratNodeToString)


main :: IO ()
main = do putStrLn ""
          putStrLn "             ^  -  ^ \n\
                   \            ( .   . ) \n\
                   \              =>;<= \n\
                   \             /     \\ \n\
                   \            |       | \n\
                   \ \n\
                   \ * Rational Arithmetic Evaluation * \n\
                   \   Example: (1%2) + (3%4) * (5%6)\n"
          putStrLn "The result of \"(1%2) + (3%4) * (5%6) - (2%1) / (1%2)\" is:"
          (print . ratNodeToString . evaluate . parse . tokenize) "(1%2) + (3%4) * (5%6) - (2%1) / (1%2)"
          putStrLn ""
          putStrLn "The result of \"(1%1) / (2%0)\" is:"
          (print . ratNodeToString . evaluate . parse . tokenize) "(1%1) / (2%0)"