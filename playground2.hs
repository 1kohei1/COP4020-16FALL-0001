-- module 7
import Data.List

numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub