quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = (quicksort lesser) ++ [x] ++ (quicksort greater)
    where
        lesser  = filter (< x) xs
        greater = filter (>= x) xs

{-
    Implemented as two level quicksorts. The first sort is sorted by length and then each equal length lists will be
    sorted by order using the quicksort implementation above.
-}
lsort :: (Ord a) => [[a]] -> [[a]]
lsort [] = []
lsort (x:xs) = (lsort lesser) ++ middle ++ (lsort greater)
    where
        lesser  = filter (\i -> length i < length x) xs
        greater = filter (\i -> length i > length x) xs
        middle = quicksort ([x] ++ (filter (\i -> length i == length x) xs))

{-
    Input must be sorted first using lsort.
-}
groupByFreq :: (Ord a) => [[a]] -> [[[a]]]
groupByFreq [] = []
groupByFreq (x:xs) = [[x] ++ sameLength] ++ (groupByFreq greaterLength)
    where
        sameLength = takeWhile (\i -> length i == length x) xs
        greaterLength = dropWhile (\i -> length i <= length x) xs

-- flattenSorted taken from previous problem
flattenSorted :: (Ord a) => [[[a]]] -> [[a]]
flattenSorted [] = []
flattenSorted [x] = x
flattenSorted (x:xs) = x ++ (flattenSorted xs)

lfsort :: (Ord a) => [[a]] -> [[a]]
lfsort [] = []
lfsort x = flattenSorted $ lsort $ groupByFreq $ lsort x

