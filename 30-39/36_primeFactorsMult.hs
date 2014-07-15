-- Solution from problem#35

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

-- generates a list of n which indicates the number of times x can be divided by n
getFactorCount :: Integer -> Integer -> [Integer]
getFactorCount x n
    | x `mod` n == 0 = n:(getFactorCount (x `quot` n) n)
    | otherwise = []

generateFactorsFromList :: Integer -> [Integer] -> [Integer]
generateFactorsFromList _ [] = []
generateFactorsFromList x (y:ys)
    | x == 1 = []
    | otherwise = factor ++ (generateFactorsFromList nextNumber ys)
    where
        factor = getFactorCount x y
        nextNumber = x `quot` (product factor)

primeFactors :: Integer -> [Integer]
primeFactors x = generateFactorsFromList x (primeGenerator x)

primeFactorsMult :: Integer -> [(Integer, Integer)]
primeFactorsMult x =
    foldl (\acc i ->
        let t = last acc
            n = fst t
            count = snd t
        in if n == i then (init acc) ++ [(n, count + 1)] else acc ++ [(i, 1)])
    [(y,1)] ys
    where
        (y:ys) = primeFactors x
