-- a
filterFirst :: (a -> Bool) -> [a] -> [a]
filterFirst p [] = []
filterFirst p (x:xs)
    | p x = x : filterFirst p xs
    | otherwise = xs
-- b
filterLast :: (a -> Bool) -> [a] -> [a]
filterLast p [] = []
filterLast p [x]        -- single-element list
    | p x       = [x]   -- p x is true, return it
    | otherwise = []    -- p x is false, remove it
filterLast p (x:xs)     --multiple element list
    | p x                       = x : filterLast p xs   -- p x is true, try next element
    | foldr1 (&&) (map p xs)    = xs                    -- p x is false, all following are true
    | otherwise                 = x : filterLast p xs   -- p x is false, but another false remains
-- c    
split :: [a] -> ([a], [a])
split [] = ([], [])
split (x:[]) = (x:[], [])
split (x:y:[]) = (x:[], y:[])
split (x:y:rest) = (x:z1, y:z2)
  where z = split rest
        z1 = fst z
        z2 = snd z
-- d
interleave :: ([a],[a]) -> [a]
interleave ([], right) = right
interleave (left, [])  = left
interleave (x:xs,y:ys) = x:y:interleave (xs,ys)
-- e
merge :: (Ord a) => ([a], [a]) -> [a]
merge ([], []) = []
merge (left, []) = left
merge ([], right) = right
merge (x:xs, y:ys)
  | x < y = x:merge (xs, y:ys)
  | otherwise = y:merge (x:xs, ys)
-- f
mergeSort :: (Ord a) => [a] -> [a]
mergeSort [] = []
mergeSort (x:[]) = x:[]
mergeSort arr = merge (leftSorted, rightSorted) -- Merge them back here
  where array = split arr -- First split into two array
        arr1 = fst array -- This is the first array
        arr2 = snd array -- This is the second array
        leftSorted = mergeSort arr1 -- Sort the first array
        rightSorted = mergeSort arr2 -- Sort the second array
