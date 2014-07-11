data EncodedData a = Single a | Multiple Int a deriving (Show)

encodeDirect :: (Eq a) => [a] -> [EncodedData a]
encodeDirect [] = []
-- TODO