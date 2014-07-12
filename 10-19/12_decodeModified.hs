data EncodedData a = Single a | Multiple Int a deriving (Show)

decodeHelper :: (Eq a) => EncodedData a -> [a]
decodeHelper (Single x) = [x]
decodeHelper (Multiple n x) = take n $ repeat x

decodeModified :: (Eq a) => [EncodedData a] -> [a]
decodeModified [] = []
decodeModified (x:xs) = (decodeHelper x) ++ (decodeModified xs)
