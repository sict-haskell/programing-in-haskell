perfects :: Int -> [Int]
perfects n = [x | x <- [1 .. n], isPerfect x]

isPerfect :: Int -> Bool
isPerfect x = x == sum (factos x)

factos :: Int -> [Int]
factos x = [y | y <- [1 .. (x `div` 2)], x `mod` y == 0]
