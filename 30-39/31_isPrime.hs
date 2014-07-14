isPrime :: Integer -> Bool
isPrime 2 = True
isPrime x = not (True `elem` (map (\i -> x `mod` i == 0) [(x - 1), (x - 2) .. 2]))
-- If true exists in the mapped list above, then it is a composite, thus we negate it.
