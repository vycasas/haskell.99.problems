data EncodedData a = Single a | Multiple Int a deriving (Show)

decodeModified :: (Eq a) => [EncodedData a] -> [a]
decodeModified [] = []
decodeModified (x:xs) = (decodeHelper x) ++ (decodeModified xs)

decodeHelper :: (Eq a) => EncodedData a -> [a]
decodeHelper (Single x) = [x]
decodeHelper (Multiple n x) = take n $ repeat x
