totient :: Integer -> Int
totient 1 = 1
totient x = length $ foldl (\acc i -> if coprime i x then acc ++ [i] else acc) [] [1 .. x]

-- Solutions from problems # 32 & 33
myGCD :: Integer -> Integer -> Integer
myGCD 0 0 = error "At least one number must be non-zero."
myGCD x 0 = myGCD 0 x
myGCD x y
    | remainder == 0 = y
    | otherwise = myGCD y remainder
    where
        remainder = mod x y

coprime :: Integer -> Integer -> Bool
coprime x y = myGCD x y == 1
