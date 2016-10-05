--
--  Team:           Haskell Coder 24
--	Team Members:   Kohei Arai
--                  Sandra Hoopes
--                  Alex Lascalbar
--                  Victoria Proetsch
--                  David Waters 
--                  Assignment:	2
--                  Course:		COP 4020
--                  Semester:	Fall 2016

--  Problem 1

--  a)   filerFirst - Removes the first element of xs which does not
satisfy the property p
filterFirst :: (a -> Bool) -> [a] -> [a]
filterFirst p [] = []
filterFirst p (x:xs)
    | p x = x : filterFirst p xs
    | otherwise = xs
    
--  b)   filterLast - Removes the last element of xs which does not
satisfy the property p
filterLast :: (a -> Bool) -> [a] -> [a]
filterLast p [] = []
filterLast p [x]                                        -- single-element list
    | p x       = [x]                                   -- p x is true, return it
    | otherwise = []                                    -- p x is false, remove it
filterLast p (x:xs)                                     -- multiple element list
    | p x                       = x : filterLast p xs   -- p x is true, try next element
    | foldr1 (&&) (map p xs)    = xs                    -- p x is false, all following are true
    | otherwise                 = x : filterLast p xs   -- p x is false, but another false remains
    
--  c)  split - Splits a list into two lists, picking elements alternately    
split :: [a] -> ([a], [a])
split [] = ([], [])
split (x:[]) = (x:[], [])
split (x:y:[]) = (x:[], y:[])
split (x:y:rest) = (x:z1, y:z2)
  where z = split rest
        z1 = fst z
        z2 = snd z
        
-- d)   interleave - Creates a list composed of alternatively selected elements from two lists
interleave :: ([a],[a]) -> [a]
interleave ([], right) = right
interleave (left, [])  = left
interleave (x:xs,y:ys) = x:y:interleave (xs,ys)

-- e)   merge - Merges two sorted lists to produce a sorted list
merge :: (Ord a) => ([a], [a]) -> [a]
merge ([], []) = []
merge (left, []) = left
merge ([], right) = right
merge (x:xs, y:ys)
  | x < y = x:merge (xs, y:ys)
  | otherwise = y:merge (x:xs, ys)
  
-- f)   mergeSort - Sorts a list by slpitting it into individual elements and then merging them tro produce a sorted list
mergeSort :: (Ord a) => [a] -> [a]
mergeSort [] = []
mergeSort (x:[]) = x:[]
mergeSort arr = merge (leftSorted, rightSorted)     -- Merge them back here
  where array = split arr                           -- First split into two array
        arr1 = fst array                            -- This is the first array
        arr2 = snd array                            -- This is the second array
        leftSorted = mergeSort arr1                 -- Sort the first array
        rightSorted = mergeSort arr2                -- Sort the second array
