module TreeLabelWithoutStateMonad where

import Store

-- label element

labelValue :: Ord a => a -> (Store a Int) -> (Int, Store a Int)
labelValue val ls
    | lookupStore val ls == Nothing = (size, newStore)
    | otherwise = (toNum maybeNum, ls)
    where 
        maybeNum = lookupStore val ls
        size = createNewLabel ls
        newStore = insertStore val size ls


data Tree a = Nil | Node a (Tree a) (Tree a) deriving (Show,Eq)

labelTree :: Ord a => Tree a -> (Store a Int) -> (Tree Int, Store a Int)
labelTree Nil ls = (Nil, ls)
-- Need to handle the first parameter is not 1
labelTree (Node val left right) ls =
  (Node labeledValue labeledLeft labeledRight, ls''')
    where (labeledValue, ls')   = labelValue val   ls
          (labeledLeft,  ls'')  = labelTree  left  ls'
          (labeledRight, ls''') = labelTree  right ls''

getLabeledTree :: Ord a => Tree a -> Tree Int
getLabeledTree tree = fst $ labelTree tree emptyStore

toNum :: Maybe a -> a
toNum (Just a) = a
