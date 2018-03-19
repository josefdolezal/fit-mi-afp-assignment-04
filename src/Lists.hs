module Lists where

-- TODO: create infinite list of pythagorean triples = (x,y,z), where x^2 + y^2 = z^2
-- TODO: use list comprehension!
-- https://en.wikipedia.org/wiki/Pythagorean_triple
-- List is ordered by x,y,z in ascending order, contains also primitive triples
pythagoreanTriples :: [(Integer, Integer, Integer)]
pythagoreanTriples = [(a, b, c) | c <- [1..], b <- [1..c-1], a <- [1..b-1], valid a b c]
    where valid a b c = a^2 + b^2 == c^2

-- TODO: create identity matrix (1 on main diagonal, 0 otherwise)
-- TODO: use list comprehension!
-- https://en.wikipedia.org/wiki/Identity_matrix
-- Note: sublists are rows
eyeMatrix :: Num a => Int -> [[a]]
eyeMatrix 0 = [[]]
eyeMatrix n = [row y n | y <- [1..n]]
    where row i n = [kroneckerDelta i x | x <- [1..n]]

kroneckerDelta :: (Eq a, Num p) => a -> a -> p
kroneckerDelta x y
    | x == y = 1
    | otherwise = 0

-- TODO: multiply matrices x and y
-- TODO: use list comprehension!
-- https://en.wikipedia.org/wiki/Matrix_multiplication
-- Note: sublists are rows
-- if wrong sizes, raise error "Incorrect matrix sizes"
matrixMultiplication :: Num a => [[a]] -> [[a]] -> [[a]]
matrixMultiplication x y = undefined
