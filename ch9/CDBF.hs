module CDBF where

import CountDown

split :: [a] -> [([a], [a])]
split [] = []
split [_] = []
split (x : xs) =
  ([x], xs)
    : [(x : ls, rs) | (ls, rs) <- split xs]

exprs :: [Int] -> [Expr]
exprs [] = []
exprs [n] = [Val n]
exprs ns =
  [ e | (ls, rs) <- split ns, 
        l <- exprs ls, 
        r <- exprs rs, 
        e <- combine l r
  ]

combine :: Expr -> Expr -> [Expr]
combine l r = [App o l r | o <- ops]

ops :: [Op]
ops = [Add, Sub, Mul, Div]
