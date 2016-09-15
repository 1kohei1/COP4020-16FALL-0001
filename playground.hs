doubleMe x = x + x
doubleUs x y = doubleMe x + doubleMe y

doubleSmallNumber x = 1 + if x > 100
                      then x
                      else x * 2

boomBangs xs = [if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]

removeUpperCase st = [c | c <- st, c `elem` ['a'..'z']]

triangles = [(a,b,c) | a <- [1..10], b <- [1..10], c <- [1..10]]

rightTriangles = [(a,b,c) | c <- [1..10], b <- [1..c], a <- [1..b], a^2+b^2==c^2]

is24 (a,b,c) = a + b + c == 24

rightTrianglesWithPerimeter24 = [x | x <- rightTriangles, is24 x]

addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z

lucky :: (Integral a) => a -> String
lucky 7 = "Lucky Seven"
lucky x = "Not luck seven"

fact :: (Integral a) => a -> a
fact 1 = 1;
fact x = if x < 1 then 0 else x * fact (x - 1)

first :: (a, b, c) -> a
first (x, _, _) = x

second :: (Num a) => (a, b, c) -> b
second (_, x, _) = x

third :: (Num a) => (a, b, c) -> c
third (_, _, x) = x

addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors a b = (fst a + fst b, snd a + snd b)

head' :: [a] -> a
head' [] = error "Can't call head on an empty list, dummy!"
head' (x:_) = x

showFirstTwoElements :: (Show a) => [a] -> String
showFirstTwoElements [] = "Empty"
showFirstTwoElements (x:[]) = show x
showFirstTwoElements (x:y:_) = show x ++ " and " ++ show y


tell :: (Show a) => [a] -> String
tell [] = "The list is empty"
tell (x:[]) = "The list has one element: " ++ show x
tell (x:y:[]) = "The list has two elements: " ++ showFirstTwoElements (x:y:[])
tell (x:y:_) = "This list is long. The irst wo elements are: " ++ showFirstTwoElements (x:y:[])

length' :: (Num b) => [a] -> b
length' [] = 0
length' (_:xs) = 1 + length' xs

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

bimTell :: (RealFloat a) => a -> a -> String
bimTell weight height
  | bmi <= skinny = "Underweight"
  | bmi <= normal = "Appropriate"
  | bmi <= fat    = "Fat"
  | otherwise   = "Not a human"
  where bmi = weight / height ^ 2
        skinny = 18.5
        normal = 25.0
        fat = 30.0

max' :: (Ord a) => a -> a -> a
max' a b
  | a < b = b
  | otherwise = a

initials :: String -> String -> String
initials firstName lastName = [f] ++ ". " ++ [l] ++ "."
  where (f:_) = firstName
        (l:_) = lastName

maximum' :: (Ord a) => [a] -> a
maximum' [] = error "maximum of empty list"
maximum' (x:[]) = x
maximum' (x:xs)
  | x < maxTail = maxTail
  | otherwise = x
  where maxTail = maximum' xs

replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' n x
  | n <= 0 = []
  | otherwise = x:replicate' (n - 1) x

take' :: (Num i, Ord i) => i -> [a] -> [a]
take' index list
  | index == 0 = [head list]
  | null list == True = []
  | index < 0 = []
  | otherwise = take' (index - 1) (tail list)

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

zip' :: [a] -> [b] -> [(a, b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x,y):zip' xs ys

elem' :: (Eq a) => a -> [a] -> Bool
elem' _ [] = False
elem' element (x:xs)
  | element == x = True
  | otherwise = elem' element xs

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
  let smallerSorted = quicksort [a | a <- xs, a <= x]
      biggerSorted = quicksort [a | a <- xs, a > x]
  in smallerSorted ++ [x] ++ biggerSorted

-- module 6

multThree :: (Num a) => a -> a -> a -> a
multThree x y z = x * y * z

compareWith100 :: (Num a, Ord a) => a -> Ordering
compareWith100 = compare 100

divideBy10 :: (Floating a) => a -> a
divideBy10 = (/10)

isUpperCase :: Char -> Bool
isUpperCase = (`elem` ['A'..'Z'])

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y: zipWith' f xs ys

flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f = g
  where g x y = f y x

--  find the largest number under 100,000 that's divisible by 3829
largestNumUptoNDivisibleByM :: (Integral a) => a -> a -> a
largestNumUptoNDivisibleByM n m
  | m == 0 = error "Division by 0 is not supported"
  | n < 1 = error "Number starts from 1. Please enter the number greater than or equal to 1"
  | length list == 0 = error "No such number is found"
  | otherwise = last list
  where list = divisibleByMInN n m

divisibleByMInN :: (Integral a, Enum a) => a -> a -> [a]
divisibleByMInN n m = filter (isDivisible m) [1..n]

isDivisible :: (Integral a) => a -> a -> Bool
isDivisible m n = n `mod` m == 0

-- find the sum of all odd squares that are smaller than 10,000
sumOfSquaresSmallerThan1000 = sum (takeWhile (<10000) (filter odd (map (^2) [1..])))  

-- for all starting numbers between 1 and 100, how many chains have a length greater than 15?
countChainLengthGreaterThan15 = length [x | x <- [1..100], isChainGreaterThan15 [x]]

-- Takes a starting number and returns if it's length is greater than 15 or not
isChainGreaterThan15 :: (Integral a) => [a] -> Bool
isChainGreaterThan15 list
  | length list > 15 = True
  | lastItem == 1 = False
  | otherwise = isChainGreaterThan15 (list ++ [nextItem])
  where lastItem = last list
        nextItem = if even lastItem then lastItem `div` 2 else lastItem * 3 + 1

-- Let's make this more generic
-- Takes two number n and m. Returns the number of chains whose Collatz sequences length is greater than m. Start number is in range of [1..n]
{-| But cannot finish solve the compile error.
countCollatzSequenceGreaterThan :: (Integral a, Enum a) => a -> x -> x
countCollatzSequenceGreaterThan n m
  | n < 1     = error "Collatz sequence start number must be a natural number"
  | otherwise = length [x | x <- [1..n], lengthIsGreaterThanM [x] m]

lengthIsGreaterThanM :: Int a => [a] -> a -> Bool
lengthIsGreaterThanM list m
  | (length list) > m = True
  | lastItem == 1     = False
  | otherwise         = lengthIsGreaterThanM (list ++ [nextItem])
  where lastItem = last list
        nextItem = if even lastItem then lastItem `div` 2 else lastItem * 3 + 1
-}

sum'' :: (Num a) => [a] -> a
sum'' xs  = foldl (\acc x -> acc + x) 0 xs

elem'' :: (Eq a) => a -> [a] -> Bool
elem'' y ys = foldl (\acc x -> if x == y then True else acc) False ys

map' :: (a -> b) -> [a] -> [b]
map' f xs = foldr (\x acc -> f x: acc) [] xs


maximum'' :: (Ord a) => [a] -> a
maximum'' = foldr1 (\x acc -> if x > acc then x else acc)

reverse'' :: [a] -> [a]
reverse'' = foldl (\acc x -> x : acc) []

product' :: (Num a) => [a] -> a
product' = foldr1 (*)

-- How many elements does it take for the sum of the roots of all natural numbers to exceed 1000?

numElementsToSumRootExeec1000 :: Int 
numElementsToSumRootExeec1000 = length (takeWhile (<1000) (scanl1 (+) (map sqrt [1..]))) + 1

-- Function application with $ is interesting. This makes so easy to read code.

-- map (\x -> negate (abs x)) [5,-3,-6,7,-3,2,-19,24]
-- This negates all of the value in the list
-- This is equal to  map (negate . abs) [5,-3,-6,7,-3,2,-19,24]  

oddSquareSum :: Integer
oddSquareSum = sum .takeWhile (<10000) . filter odd . map (^2) $ [1..]









