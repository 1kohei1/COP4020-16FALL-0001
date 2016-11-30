-- define a polymorphic binary tree in Haskell

data Tree a = Nil | Node a (Tree a) (Tree a) deriving (Show, Eq)

-- :k is a command to display what type that is

-- :k Tree
Tree :: * -> *

-- :k Tree Int
Tree Int :: *

-- :k Maybe 
Maybe :: * -> *

-- :k Maybe String
Maybe String :: *

-- example of type constructor with kind * -> * -> *
-- Either

:k Either
Either :: * -> * -> *

:k Either String
Either String :: * -> *

-- Maybe this could be on the test. Define Either
data Either a b = Left a || Right b 

-- signature of filter and map. There are heart of functional programming
-- He could ask question like implement filter and map

-- :t filter    
filter :: (a 0> Bool) -> [a] -> [a]

-- :t map
map :: (a -> b) -> [a] -> b

-- :t zipWith
zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]

-- basic functions in functional programming
-- filter, map, zip, zipWith

-- What are the two basic function that you have to implement for a monad?
-- return :: a -> m a
-- (>>=) :: m a -> ( a -> m b) -> m b

-- give examples of classes in Haskell
-- Eq, Ord, Show, Num, Integral

-- What is a class constraint?

-- Problem 2:

occursNum :: Eq a => a -> Tree a -> Integer
occursNum _ Nil = 0
occursNum x (Node y left right) = 
    | x == y = 1 + (occursNum x left) + (occursNum x right)
    | otherwise = (occursNum x left) + (occursNum x right)

occurs :: Eq a => a -> Tree a -> Bool
occurs _ Nil = False
occurs x (Node y left right) = (x == y) || (occurs x left) || (occurs x right)
-- We can use occursNum in this function, but using it might end up more computations

quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort [x] = [x]
quicksort (x:xs) = (quickSort left) ++ [x] ++ (quickSort right)
  where
    left  = flter (<=x) xs
    right = filter (>x) xs

    -- we can rewrite this with 