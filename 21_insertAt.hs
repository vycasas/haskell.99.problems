insertAt :: a -> [a] -> Int -> [a]
insertAt i [] _ = [i]
insertAt i x n = (take (n - 1) x) ++ [i] ++ (drop (n - 1 ) x)
