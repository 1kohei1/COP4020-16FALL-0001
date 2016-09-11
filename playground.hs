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







