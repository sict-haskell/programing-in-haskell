_pow :: Int -> Int -> Int
_pow _ 0 = 1
_pow x 1 = x
_pow x y = x * (_pow x (y - 1))
