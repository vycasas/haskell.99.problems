myLast :: [a] -> a
myLast [] = error "Cannot get last element from an empty list."
myLast [x] = x
myLast (_:xs) = myLast xs
