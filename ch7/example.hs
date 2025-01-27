_rev :: [a] -> [a]
_rev  = foldr (\next acc -> acc ++ [next] ) []  

x :: Bool
x = foldr (&&) False (repeat True)
