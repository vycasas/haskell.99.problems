slice :: [a] -> Int -> Int -> [a]
slice x s e = drop (s - 1) $ take e x
