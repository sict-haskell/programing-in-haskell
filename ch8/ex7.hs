import Data.Type.Bool (Not)

class E a where
  e :: a -> a -> Bool
  ne :: a -> a -> Bool
  ne a b = not (e a b)

instance (E a) => E (Maybe a) where
  e :: (E a) => Maybe a -> Maybe a -> Bool
  e Nothing Nothing = True
  e (Just a) (Just b) = e a b
  e _ _ = False

instance (E a) => E [a] where
  e [] [] = True
  e [a] [b] = e a b
  e xs [] = False
  e [] xs = False
  e (x : xs) (y : ys) = e x y && e xs ys
