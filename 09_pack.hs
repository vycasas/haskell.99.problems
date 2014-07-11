packHelper :: (Eq a) => [[a]] -> [a] -> [a] -> [[a]]
packHelper _ _ [] = []
packHelper r i (x:xs)
    | xs == [] = r ++ [(i ++ [x])]
    | x `elem` i = packHelper r (i ++ [x]) xs
    | otherwise = packHelper (r ++ [i]) [x] xs

pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack [x] = [[x]]
pack (x:xs) = packHelper [] [x] xs
