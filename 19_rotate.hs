rotate :: [a] -> Int -> [a]
rotate [] _ = []
rotate x n
    | n >= 0 = drop n x ++ take n x
    | otherwise = let nn = (length x) + n in drop nn x ++ take nn x
