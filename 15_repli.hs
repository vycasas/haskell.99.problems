repli :: [a] -> Int -> [a]
repli x n = foldr (\i acc -> (take n $ repeat i) ++ acc) [] x
