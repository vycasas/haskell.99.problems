import Data.Monoid

goldbachList :: Integer -> Integer -> [(Integer, Integer)]
goldbachList x y
    | x <= 2 = error "Number must be greater than 2."
    | x > y = goldbachList y x
    | y == 3 = []
    | otherwise = map goldbach evenNumList
        where
            evenNumList = filter even [x .. y]

-- slow on big numbers (updated using DiffList to improve performance)
goldbachList' :: Integer -> Integer -> Integer -> [(Integer, Integer)]
goldbachList' x y z = filter (\(a, _) -> a >= z) $ goldbachList x y

-- solution from problem # 40
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
primeGenerator :: Integer -> [Integer]
primeGenerator x
    | x < 2 = []
    | x == 2 = [2]
    | otherwise = compositeRemover [2 .. x] (toDiffList [])

-- difference list implementation from Learn You a Haskell chapter 13
newtype DiffList a = DiffList { getDiffList :: [a] -> [a] }
toDiffList :: [a] -> DiffList a
toDiffList xs = DiffList (xs++)
fromDiffList :: DiffList a -> [a]
fromDiffList (DiffList f) = f []

instance Monoid (DiffList a) where
    mempty = DiffList (\xs -> [] ++ xs)
    (DiffList f) `mappend` (DiffList g) = DiffList (\xs -> f (g xs))

compositeRemover :: [Integer] -> (DiffList Integer) -> [Integer]
compositeRemover [] primeDiffList = fromDiffList primeDiffList
compositeRemover (x:xs) primeList =
    compositeRemover
        (fromDiffList (foldl (\acc i -> if i `mod` x == 0 then acc else acc `mappend` (toDiffList [i])) (toDiffList []) xs))
        (primeList `mappend` (toDiffList [x]))
