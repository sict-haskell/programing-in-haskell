merge :: Ord a => [a] -> [a] -> [a]
merge [] x = x
merge x [] = x
merge (x:xs) (y:ys) = if x < y then x : (merge xs (y :ys))
                      else y : (merge (x:xs) ys)


twice :: (a -> a) -> a -> a
twice f x = f (f x)

