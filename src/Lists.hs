module Lists where

import qualified Data.List as List

-- https://en.wikipedia.org/wiki/Pythagorean_triple
-- List is ordered by x,y,z in ascending order, contains also primitive triples
pythagoreanTriples :: [(Integer, Integer, Integer)]
pythagoreanTriples = [(a, b, c) | c <- [1..], b <- [1..c-1], a <- [1..b-1], valid a b c]
    where valid a b c = a^2 + b^2 == c^2

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

-- https://en.wikipedia.org/wiki/Matrix_multiplication
-- Note: sublists are rows
-- if wrong sizes, raise error "Incorrect matrix sizes"
matrixMultiplication :: Num a => [[a]] -> [[a]] -> [[a]]
matrixMultiplication x y 
    | length (List.transpose x) == length y = [[reduce a b | b <- List.transpose y] | a <- x]
    | otherwise = error "Incorrect matrix sizes"
        where reduce x y = sum $ zipWith (*) x y
