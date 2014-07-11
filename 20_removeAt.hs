removeAt :: Int -> [a] -> (a, [a])
removeAt _ [] = error "Empty list"
removeAt n x = (itemAtIndex, itemRemoved)
    where
        itemAtIndex = last $ take n x
        itemRemoved = (init $ take n x) ++ (drop n x)
