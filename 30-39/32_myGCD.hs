-- The question only says about positive integers, so I used the Integer type
myGCD :: Integer -> Integer -> Integer
myGCD 0 0 = error "At least one number must be non-zero."
myGCD x 0 = myGCD 0 x
myGCD x y
    | remainder == 0 = y
    | otherwise = myGCD y remainder
    where
        remainder = mod x y
