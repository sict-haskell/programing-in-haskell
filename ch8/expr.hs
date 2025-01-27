data Expr = Val Int | Add Expr Expr

type Cont = [Op]

data Op = EVAL Expr | ADD Int

eval :: Expr -> Cont -> Int
eval (Val n) c = exec c n
eval (Add x y) c = eval x (EVAL y : c)

exec :: Cont -> Int -> Int
exec [] n = n
exec (EVAL y : c) n = eval y (ADD n : c)
exec (ADD n : c) m = exec c (n + m)

value :: Expr -> Int
value e = eval e []

folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
folde f _ (Val a) = f a
folde f g (Add a b) = g (folde f g a) (folde f g b)

eval6 :: Expr -> Int
eval6 = folde id (+)

size6 :: Expr -> Int
-- size6 (Val _) = 1
-- size6 (Add a b) = size6 a + size6 b
size6 = folde (const 1) (+)
