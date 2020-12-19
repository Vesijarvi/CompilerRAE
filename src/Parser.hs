module Parser where

import Lexer ( Token(..), Operator(Div, Add, Sub, Mul) ) 

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
                let (termTree, toks'') = term (accept toks')
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