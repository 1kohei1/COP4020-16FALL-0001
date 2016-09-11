head' :: [a] -> a
head' [] = error "headless"
--head' (x:xs) = x
head' (x:xs) = x

-- Cut the first element
tail' :: [a] -> [a]
tail' [] = error "empty"
tail' (_:x) = x