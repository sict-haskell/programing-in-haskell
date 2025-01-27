halve :: [a] -> ([a], [a])
halve [x, y] = ([x], [y])
halve xs = (l, r)
  where
    lth = (length xs) `div` 2
    l = take lth xs
    r = drop lth xs
