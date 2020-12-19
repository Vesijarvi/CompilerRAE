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

------ End Lexer ------

data Tree = SumNode Operator Tree Tree
          | ProdNode Operator Tree Tree
          | RatNode Int Int   -- node to save ratioal number
    deriving Show

{-
Expression <- Term Op(+/) Expression
            | Term
term       <- Factor Op(*/) Term
            | Factor
factor     <- LBR Num % Num LBR
            | [+-] factor
            | LBR Expression RBR
-}

{-
    Expr <- Ractioal
          | Expr op[+-] Expr
          | Expr op[*/] Expr
          | '(' Expr ')' 
-}

------ Start parse ------
parse :: [Token] -> Tree
parse toks = let (tree, toks') = expression toks
             in 
                  if null toks' 
                  then tree
                  else error $ "Leftover tokens: " ++ show toks'

expression :: [Token] -> (Tree, [Token])
expression toks = 
    let (termTree, toks') = term toks
    in
        case lookAhead toks' of
            -- Tern op[+-] Expression
            (TokOp op) | elem op [Add, Sub] ->
                let (expTree, toks'') = expression (accept toks')
                in (SumNode op termTree expTree, toks'')
            -- Term
            _ -> (termTree, toks')

term :: [Token] -> (Tree, [Token])
term toks = 
    let (factTree, toks') = factor toks
    in 
        case lookAhead toks' of
            -- Term op[*/] Expression
            (TokOp op) | elem op [Mul, Div] -> 
                let (termTree, toks'') = expression (accept toks')
                in (ProdNode op factTree termTree, toks'')
            _ -> (factTree, toks')

factor :: [Token] -> (Tree, [Token])
factor toks = 
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
------ End parse ------

------ Start Evaluate ------
evaluate :: Tree -> Tree
evaluate (RatNode x y) = RatNode x y
evaluate (SumNode op left right) = 
    let (RatNode lft_x lft_y)  = evaluate left 
        (RatNode rght_x rght_y) = evaluate right 
    in
        case op of 
            Add -> 
                let a = lft_x * rght_y + rght_x * lft_y
                    b = lft_y * rght_y
                    d = gcd a b 
                in RatNode (div a d) (div b d)
            Sub -> 
                let a = lft_x * rght_y - rght_x * lft_y
                    b = lft_y * rght_y
                    d = gcd a b 
                in RatNode (div a d) (div b d)
            _ -> error $ "Found somthing weird: " ++ show op

evaluate (ProdNode op left right) = 
    let (RatNode lft_x lft_y)  = evaluate left 
        (RatNode rght_x rght_y) = evaluate right 
    in
        case op of
            Mul -> 
                let 
                    a = (lft_x * rght_x)
                    b = (lft_y * rght_y)
                    d = gcd a b
                in RatNode (div a d) (div b d) 
            Div -> 
                let -- ( a % b ) / ( c % d)
                    a = (lft_x * rght_y)
                    b = (rght_x * lft_y)
                    d = gcd a b
                in 
                    if b == 0
                    then error "divide by zero"
                    else RatNode (div a d) (div b d)
            _ -> error $ "Found somthing weird: " ++ show op

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
          -- inputString <- getLine
          -- putStrLn ("Your input is \"" ++ inputString ++ "\"!")
          -- (print . tokenize) "(3)" 
          -- (print . parse . tokenize) "(1%2) + (3%4) * (5%6) - (2%1) / (1%2)"
          -- putStrLn "The result of \"(1%2) + (3%4) * (5%6) - (2%1) / (1%2)\" is:"

          (print . parse . tokenize) "(1%2) + (3%4) * (5%6) - (2%1) / (1%2)"
          (print . evaluate . parse . tokenize) "(1%2) + (3%4) * (5%6) - (2%1) / (1%2)"