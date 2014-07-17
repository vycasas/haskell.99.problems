combinations :: Int -> [a] -> [[a]]
combinations n x =
    let allPossibles = foldr (\i acc -> combinationsGenerator i acc) [] x
    in [i | i <- allPossibles, length i == n]

-- This function generates all the possile combinations without repeatitions.
combinationsGenerator :: a -> [[a]] -> [[a]]
combinationsGenerator x [] = [[x]]
combinationsGenerator x y = [[x]] ++ (map ([x]++) y) ++ y
