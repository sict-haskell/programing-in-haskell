data Tree a = Leaf a | Node (Tree a) (Tree a)

leaves :: (Num a1) => Tree a2 -> a1
leaves (Leaf _) = 1
leaves (Node l r) = leaves l + leaves r

balanced :: Tree a -> Bool
balanced (Leaf _) = True
balanced (Node l r) =
  abs (leaves l - leaves r) <= 1
    && balanced l
    && balanced r

halve :: [a] -> ([a], [a])
halve [] = ([], [])
halve [x] = ([x], [])
halve xs = splitAt x xs
  where
    x = length xs `div` 2

balance :: [a] -> Tree a
balance [x] = Leaf x
balance xs = Node (balance f1) (balance f2)
  where
    s = halve xs
    f1 = fst s
    f2 = snd s
