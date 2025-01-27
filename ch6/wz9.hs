import Text.Printf (errorBadArgument)

_sum :: (Fractional a) => [a] -> a
_sum [] = 0
_sum (x : xs) = x + _sum xs

_take :: Int -> [a] -> [a]
_take 0 _ = []
_take _ [] = []
_take n (x : xs) = x : _take (n - 1) xs

_last :: [a] -> a
_last [] = errorBadArgument
_last [x] = x
_last (_ : xs) = _last xs
