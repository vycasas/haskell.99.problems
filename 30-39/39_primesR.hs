-- solution is based from my prime generator at problem#35
-- prime generator implemented using Sieve of Eratosthenes
compositeRemover :: [Integer] -> [Integer] -> [Integer]
compositeRemover [] primeList = primeList
compositeRemover (x:xs) primeList =
    compositeRemover (foldl (\acc i -> if i `mod` x == 0 then acc else acc ++ [i]) [] xs) (primeList ++ [x])

primesR :: Integer -> Integer -> [Integer]
primesR x y
    | x < 2 = primesR 2 y
    | x > y = primesR y x
    | otherwise = [i | i <- (compositeRemover [2 .. y] []), i >= x]
