import Data.Sequence (Seq (Empty))

isChoice :: (Eq a) => [a] -> [a] -> Bool
isChoice [] _ = True
isChoice (x : xs) [] = False
isChoice (t : ts) xs = currentRes && nextRes
  where
    res = removeOccurrence t xs
    test Nothing = False
    test _ = True
    currentRes = test res
    nextRes = let (Just n) = res in isChoice ts n

removeOccurrence :: (Eq a) => a -> [a] -> Maybe [a]
removeOccurrence _ [] = Nothing
removeOccurrence a (x : xs) = if a == x then Just xs else removeOccurrence a xs
