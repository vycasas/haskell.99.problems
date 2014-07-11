dropEvery :: [a] -> Int -> [a]
dropEvery [] _ = []
dropEvery x n = nextDrop ++ (if mustDrop then [] else [last x])
    where
        mustDrop = length x `mod` n == 0
        nextDrop = dropEvery (init x) n
