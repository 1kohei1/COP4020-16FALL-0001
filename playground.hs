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