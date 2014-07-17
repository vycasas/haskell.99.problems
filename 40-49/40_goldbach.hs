goldbach :: Integer -> (Integer, Integer)
goldbach x
    | x <= 2 = error "Number must be greater than 2."
    | x `mod` 2 /= 0 = error "Number must be even."
    | otherwise = (n1, n2)
        where
            primesList = primeGenerator x
            n2 = foldr (\i acc -> if acc == 0 && ((x - i) `elem` primesList) then i else acc) 0 primesList
            n1 = x - n2

-- prime generator implemented using Sieve of Eratosthenes
compositeRemover :: [Integer] -> [Integer] -> [Integer]
compositeRemover [] primeList = primeList
compositeRemover (x:xs) primeList =
    compositeRemover (foldl (\acc i -> if i `mod` x == 0 then acc else acc ++ [i]) [] xs) (primeList ++ [x])

primeGenerator :: Integer -> [Integer]
primeGenerator x
    | x < 2 = []
    | x == 2 = [2]
    | otherwise = compositeRemover [2 .. x] []
