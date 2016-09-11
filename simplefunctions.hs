max'' :: (Ord a) => a -> a -> a
max'' x y
  | (x >= y) = x
  | otherwise = y

compare' :: (Ord a) => a -> a -> String
compare' x y
  | x == y = "eqal"
  | x > y = "bigger"
  | otherwise = "smaller"

describeNum :: Int -> String
describeNum x
  | x == 0 = "zero"
  | x == 1 = "one"
  | otherwise = "big number"

describeNum' :: Int -> String
describeNum' 0 = "zero"
describeNum' 1 = "one"
describeNum' _ = "big number"

pair :: Int -> Int -> String
pair 1 _ = "first arg is 1"
pair _ 1 = "second arg is 1"
pair _ _ = "whatever"