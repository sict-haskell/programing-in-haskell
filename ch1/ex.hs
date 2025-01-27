tP :: (Num x) => [x] -> x
tP [] = 1
tP (x:xs) = x * (tP xs)

factorial :: (Num a, Enum a) => a -> a
factorial n = product [1..n]

fuckYPP :: String
fuckYPP = "YPP"

