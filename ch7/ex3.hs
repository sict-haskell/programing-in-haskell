ex3 :: (a -> Bool) -> (a -> b) -> [a] -> [b]
ex3 p f = foldr (ex3_helper p f) []

ex3_helper :: (a -> Bool) -> (a -> b) -> a -> [b] -> [b]
ex3_helper p f a bs =
  if p a
    then f a : bs
    else bs
