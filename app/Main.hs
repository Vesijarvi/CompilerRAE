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

tokenizer :: String -> [Token]
tokenizer [] = []
tokenizer (c : cs) 
    | elem c "+-*/" = TokOp (operator c) : tokenizer cs
    | c == '%'   = TokSep : tokenizer cs
    | c == '('   = TokLParen : tokenizer cs
    | c == ')'   = TokRParen : tokenizer cs
    | isDigit c  = number c cs
    | isSpace c  = tokenizer cs
    | otherwise  = error $ "Cannot tokenize " ++ [c]

number :: Char -> [Char] -> [Token]
number c cs = 
   let (digs, cs') = span isDigit cs in
   TokNum (read (c : digs)) : tokenizer cs'

data Tree = SumNode Operator Tree Tree
          | ProdNode Operator Tree Tree
          | RatNode Int Int   -- node to save ratioal number
    deriving Show

------ End Lexer ------


{-
Expression <- Term Op(*/) Expression
            | Term Op(+/) Expression
            | Term
Term       <- LBR Num % Num LBR
            | LBR Expression RBR
     
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
    let (termTree, toks') = term toks
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

-- Term       <- LBR Int % Int LBR
--             | LBR Expression RBR   ! NOT IMPLEMENTED YET
term :: [Token] -> (Tree, [Token])
term toks = 
    case lookAhead toks of
        TokLParen -> 
            let toks' = accept toks
            in case lookAhead toks' of 
                -- ( Num % Num )
                (TokNum x) -> 
                    let toks'' = accept toks'
                    in case lookAhead toks'' of
                        TokSep -> 
                            let toks''' = accept toks''
                            in case lookAhead toks''' of
                                (TokNum y) ->
                                    let toks'''' = accept toks'''
                                    in case lookAhead toks'''' of 
                                        TokRParen -> (RatNode x y, accept toks'''')
                                        _ -> error $ "Parse error on Token: " ++ show toks''''
                                _ -> error $ "Parse error on Token: " ++ show toks'''
                        _ -> error $ "Parse error on Token: " ++ show toks''
                -- ( exp )
                _ -> 
                    let (expTree, toks'') = expression (accept toks')
                    in 
                       if lookAhead toks'' == TokRParen 
                       then (expTree, accept toks'')
                       else error "Missing right parenthesis"

        _ -> error $ "Parse error on Token: " ++ show toks

lookAhead :: [Token] -> Token
lookAhead [] = TokEnd
lookAhead (c:cs) = c

accept :: [Token] -> [Token]
accept [] = error "Nothing to accept"
accept (t:ts) = ts
------ End Parser ------

------ Start Evaluate ------

-- data Tree = SumNode Operator Tree Tree
--           | ProdNode Operator Tree Tree
--           | RatNode Tree Tree   -- node to save ratioal number
--           | NumNode Int

evaluate :: Tree -> Tree
evaluate (RatNode x y) = RatNode x y
evaluate (SumNode op left right) = 
    let (RatNode lft_x lft_y)  = evaluate left 
        (RatNode rght_x rght_y) = evaluate right 
    in
        case op of 
            Add -> RatNode (lft_x * rght_y + rght_x * lft_y) (lft_y * rght_y)
            Sub -> RatNode (lft_x * rght_y - rght_x * lft_y) (lft_y * rght_y) 

evaluate (ProdNode op left right) = 
    let (RatNode lft_x lft_y)  = evaluate left 
        (RatNode rght_x rght_y) = evaluate right 
    in
        case op of 
            Add -> RatNode (lft_x * rght_x) (lft_y * rght_y)
            Sub -> RatNode (lft_x / rght_x) (lft_y / rght_y) 

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
          -- (print . tokenizer) "(3)" 
          (print . parser . tokenizer) "(1%2) + (3%4) * (5%6) - (2%1) / (1%2)"  