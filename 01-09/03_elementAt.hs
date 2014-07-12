elementAt :: [a] -> Int -> a
elementAt _ 0 = error "Invalid number."
elementAt [] _ = error "Empty list."
elementAt (x:_) 1 = x
elementAt (_:xs) n = elementAt xs (n - 1)
