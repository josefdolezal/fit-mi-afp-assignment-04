module Lists where

-- TODO: create infinite list of pythagorean triples = (x,y,z), where x^2 + y^2 = z^2
-- TODO: use list comprehension!
-- https://en.wikipedia.org/wiki/Pythagorean_triple
-- List is ordered by x,y,z in ascending order, contains also primitive triples
pythagoreanTriples :: [(Integer, Integer, Integer)]
pythagoreanTriples = undefined

-- TODO: create identity matrix (1 on main diagonal, 0 otherwise)
-- TODO: use list comprehension!
-- https://en.wikipedia.org/wiki/Identity_matrix
-- Note: sublists are rows
eyeMatrix :: Num a => Int -> [[a]]
eyeMatrix n = undefined

-- TODO: multiply matrices x and y
-- TODO: use list comprehension!
-- https://en.wikipedia.org/wiki/Matrix_multiplication
-- Note: sublists are rows
-- if wrong sizes, raise error "Incorrect matrix sizes"
matrixMultiplication :: Num a => [[a]] -> [[a]] -> [[a]]
matrixMultiplication x y = undefined
