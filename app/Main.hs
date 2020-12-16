module Main where

import Lib

main :: IO ()
main = do putStrLn "             ^  -  ^\n\
                   \            ( .   . )\n\
                   \              =>;<=\n\
                   \             /     \\\n\
                   \            |       |\n\
                   \ * Rational Arithmetic Evaluation *\n\
                   \   Example: (1%2) + (3%4) * (5%6)"
          inputString <- getLine
          putStrLn ("Your input is \"" ++ inputString ++ "\"!")