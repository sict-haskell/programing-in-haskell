safetailA :: [a] -> [a]
safetailA xs = if null xs then [] else tail xs

safeTailB :: [a] -> [a]
safeTailB xs
  | null xs = []
  | otherwise = tail xs

safeTailC :: [a] -> [a]
safeTailC [] = []
safeTailC (x : xs) = xs
