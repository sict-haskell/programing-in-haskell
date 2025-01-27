cur :: ((a, b) -> c) -> (a -> b -> c)
cur f a b = f (a, b)

test :: (Int, Int) -> Int
test (a, b) = a + b

uncur :: (a -> b -> c) -> ((a, b) -> c)
uncur f = g
  where
    g (a, b) = f a b
