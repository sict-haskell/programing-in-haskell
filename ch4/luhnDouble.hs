luhnDouble :: Int -> Int
luhnDouble x =
  let tmp = x * 2
   in if tmp > 9 then tmp - 9 else tmp

luhn :: Int -> Int -> Int -> Int -> Bool
luhn x y z t = allSum `mod` 10 == 0
  where
    allSum = sum [luhnDouble x, y, luhnDouble z, t]

altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap _ _ [] = []
altMap f1 f2 (x : xs) = f1 x : altMap f2 f1 xs

luhn' :: [Int] -> Bool
luhn' [] = False
luhn' xs = allSum `mod` 10 == 0
  where
    allSum = sum (altMap luhnDouble id xs)
