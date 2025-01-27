ex1 :: Integer
ex1 = sum [x ^ 2 | x <- [1 .. 100]]

grid :: Int -> Int -> [(Int, Int)]
grid m n = [(x, y) | x <- [0 .. m], y <- [0 .. n]]

square :: Int -> [(Int, Int)]
square n = [(a, b) | (a, b) <- grid n n, a /= b]

_replicate :: Int -> a -> [a]
_replicate t x = [x | _ <- [1 .. t]]
