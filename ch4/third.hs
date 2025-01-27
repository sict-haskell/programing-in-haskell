thirdHT :: [a] -> a
thirdHT = head . tail . tail

thirdId :: [a] -> a
thirdId = (!! 2)


thirdPT :: [a] -> a
thirdPT (x : y : z : xs) = z
 