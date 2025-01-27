_and :: [Bool] -> Bool
_and = and

_concat :: [[a]] -> [a]
_concat [] = []
_concat [x] = x
_concat (x : xs) = x ++ _concat xs

_replicate :: Int -> a -> [a]
_replicate 0 a = []
_replicate n a = a : _replicate (n - 1) a

(!!!) :: [a] -> Int -> a
(!!!) [] _ = error "too large"
(!!!) (x : xs) 0 = x
(!!!) (x : xs) n = (!!!) xs (n - 1)

_elem :: (Eq a) => a -> [a] -> Bool
_elem _ [] = False
_elem a (x : xs) = (a == x) || _elem a xs
