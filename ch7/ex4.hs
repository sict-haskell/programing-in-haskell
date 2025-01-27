dec2int :: [Int] -> Int
dec2int = foldl (\acc next -> acc * 10 + next) 0
