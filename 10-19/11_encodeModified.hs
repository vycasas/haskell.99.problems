-- pack solution from problem#9
packHelper :: (Eq a) => [[a]] -> [a] -> [a] -> [[a]]
packHelper _ _ [] = []
packHelper r i (x:xs)
    | xs == [] = r ++ [(i ++ [x])]
    | x `elem` i = packHelper r (i ++ [x]) xs
    | otherwise = packHelper (r ++ [i]) [x] xs

pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack [x] = [[x]]
pack (x:xs) = packHelper [] [x] xs

-- encode solution from problem#10
encode :: (Eq a) => [a] -> [(Int, a)]
encode [] = []
encode x = [(length i, head i) | i <- (pack x)]

data EncodedData a = Single a | Multiple Int a deriving (Show)

createEncodeResult :: (Eq a) => (Int, a) -> EncodedData a
createEncodeResult (1, a) = Single a
createEncodeResult (n, a) = Multiple n a

encodeModifiedHelper :: (Eq a) => [(Int, a)] -> [EncodedData a]
encodeModifiedHelper [] = []
encodeModifiedHelper x = [createEncodeResult i | i <- x]

encodeModified :: (Eq a) => [a] -> [EncodedData a]
encodeModified = encodeModifiedHelper . encode
