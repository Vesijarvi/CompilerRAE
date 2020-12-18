module Main where

import Data.Char

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

data Tree = SumNode Operator Tree Tree
          | ProdNode Operator Tree Tree
          | RatNode Tree Tree   -- node to save ratioal number
          | NumNode Int
    deriving Show

------ End Lexer ------


{-
Expression <- Term Op(*/) Expression
            | Term Op(+/) Expression
            | Term
Term       <- LBR Factor % Factor LBR
            | LBR Expression RBR
Factor     <- Num 
     
-}

------ Start Parser ------

parser :: [Token] -> Tree
parser toks = let (tree, toks') = expression toks
              in 
                  if null toks' 
                  then tree
                  else error $ "Leftover tokens: " ++ show toks'

expression :: [Token] -> (Tree, [Token])
expression toks = 
    let (termTree, toks') = factor toks
    in 
        case lookAhead toks' of
            -- Term op[+-] Expression
            (TokOp op) | elem op [Mul, Div] -> 
                let (termTree', toks'') = expression (accept toks')
                in (SumNode op termTree termTree', toks'')
            -- Tern op[*/] Expression
            (TokOp op) | elem op [Add, Sub] ->
                let (termTree', toks'') = expression (accept toks')
                in (SumNode op termTree termTree', toks'')
            -- Term
            _ -> (termTree, toks')

-- Term       <- LBR Factor % Factor LBR
--             | LBR Expression RBR
term :: [Token] -> (Tree, [Token])
term toks = 
    case lookAhead toks of
        TokLParen ->
            let (factTree, toks') = factor (accept toks) 
            in
                case lookAhead toks' of
                    TokSep -> 
                        let (factTree', toks'') = factor (accept toks')
                        in
                            case lookAhead toks'' of 
                                TokRParen -> (RatNode factTree factTree', accept toks'')
                                _ -> error $ "Parse error on Token: " ++ show toks'' 
                    _ -> error $ "Parse error on Token: " ++ show toks'
        _ -> error $ "Parse error on Token: " ++ show toks

-- Factor     <- Num 
factor :: [Token] -> (Tree, [Token])
factor toks = 
    case lookAhead toks of 
        (TokNum x) -> (NumNode x, accept toks)
        _ -> error $ "Parse error on token: " ++ show toks

lookAhead :: [Token] -> Token
lookAhead [] = TokEnd
lookAhead (c:cs) = c

accept :: [Token] -> [Token]
accept [] = error "Nothing to accept"
accept (t:ts) = ts

------ End Parser ------

main :: IO ()
main = do putStrLn "             ^  -  ^ \n\
                   \            ( .   . ) \n\
                   \              =>;<= \n\
                   \             /     \\ \n\
                   \            |       | \n\
                   \ * Rational Arithmetic Evaluation * \n\
                   \   Example: (1%2) + (3%4) * (5%6)"
          -- inputString <- getLine
          -- putStrLn ("Your input is \"" ++ inputString ++ "\"!")
          -- (print . tokenize) "(3)" 
          (print . parser . tokenize) "(3)"  