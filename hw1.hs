-- Problem 1
computePolygonArea :: [(Double, Double)] -> Double
computePolygonArea [] = error "No points in the list. ERROR!!"
computePolygonArea [(_, _)] = error "Only one point. ERROR!!"
-- What if there are only two points? Isn't that also another error case since there is no area?
-- When it reaches here, it is guaranteed to have two elements in the list.
computePolygonArea all@(a:b:[]) = computeRecursive all / 2
computePolygonArea all@(a:b:x) = (det (last x) a + computeRecursive all) / 2

computeRecursive :: [(Double, Double)] -> Double
computeRecursive (a:b:[]) = det a b
computeRecursive (a:b:x) = det a b + computeRecursive (b:x)

det :: (Double,Double) -> (Double,Double) -> Double
det (a, b) (c, d) = a * d - b * c

-- Problem 2
-- Learn how to declare custom type
