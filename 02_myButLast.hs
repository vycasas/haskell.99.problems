myButLast :: [a] -> a
myButLast [] = error "List is empty."
myButLast [x] = error "List has only one element."
myButLast [x, _] = x
myButLast (_:xs) = myButLast xs

