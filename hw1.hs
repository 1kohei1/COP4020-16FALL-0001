-- Problem 1
computePolygonArea :: [(Double, Double)] -> Double
computePolygonArea [] = error "No points in the list. ERROR!!"
computePolygonArea [(_, _)] = error "Only one point. ERROR!!"
-- What if there are only two points? Isn't that also another error case since there is no area?
-- When it reaches here, it is guaranteed to have two elements in the list.
computePolygonArea x = computeRecursive x / 2

computeRecursive :: [(Double, Double)] -> Double
computeRecursive (a:b:[]) = det a b
computeRecursive (a:b:x) = det a b + det (last x) a + computeRecursive2 (b:x)

computeRecursive2 :: [(Double, Double)] -> Double
computeRecursive2 (a:b:[]) = det a b
computeRecursive2 (a:b:x) = det a b +  computeRecursive2 (b:x)

det :: (Double,Double) -> (Double,Double) -> Double
det (a, b) (c, d) = a * d - b * c
