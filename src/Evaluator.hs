module Evaluator where

import Lexer ( Operator(Div, Add, Sub, Mul) ) 
import Parser ( Tree(..) )

------ Start Evaluate ------
evaluate :: Tree -> Tree
evaluate (RatNode x y) = RatNode x y
evaluate (SumNode op left right) = 
    let (RatNode lft_x lft_y)  = evaluate left 
        (RatNode rght_x rght_y) = evaluate right 
    in 
        if lft_y == 0 || rght_y ==0
        then error "divide by zero"
        else
            case op of 
                -- (lft_x % lft_y) op (rght_x % rght_y)
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
        if lft_y == 0 || rght_y ==0
        then error "divide by zero"
        else 
            case op of
                Mul -> 
                    let 
                        a = (lft_x * rght_x)
                        b = (lft_y * rght_y)
                        d = gcd a b
                    in RatNode (div a d) (div b d) 
                Div -> 
                    let -- ( a % b ) / ( c % d )
                        a = (lft_x * rght_y)
                        b = (rght_x * lft_y)
                        d = gcd a b
                    in 
                        if b == 0
                        then error "divide by zero"
                        else RatNode (div a d) (div b d)
                _ -> error $ "Found somthing weird: " ++ show op

ratNodeToString :: Tree -> String 
ratNodeToString node =
    case node of 
        RatNode a b -> "(" ++ show a ++ "%" ++ show b ++ ")"
        _ -> error "This is not a RatNode"