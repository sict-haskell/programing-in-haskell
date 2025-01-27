_all :: (a -> Bool) -> [a] -> Bool
_all f [] = True
_all f xs = _all' f True xs

_all' :: (a -> Bool) -> Bool -> [a] -> Bool
_all' _ acc [] = acc
_all' _ False _ = False
_all' f acc (x : xs) = _all' f (acc && f x) xs

_takeWhile :: (a -> Bool) -> [a] -> [a]
_takeWhile _ [] = []
_takeWhile f (x : xs) =
  if f x
    then x : _takeWhile f xs
    else _takeWhile f xs
