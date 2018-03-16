module ListsSpec (spec) where

import Test.Hspec
import Control.Exception

import Lists

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

matrixA = [[1,2],[3,4]]
matrixB = [[5,5],[1,7]]
matrixC = [[3,1,2],[7,2,1]]
matrixD = [[3,1],[2,1],[1,3]]
matrixE = [[1,2,3,4]]

eye10 = [
            [1,0,0,0,0,0,0,0,0,0],
            [0,1,0,0,0,0,0,0,0,0],
            [0,0,1,0,0,0,0,0,0,0],
            [0,0,0,1,0,0,0,0,0,0],
            [0,0,0,0,1,0,0,0,0,0],
            [0,0,0,0,0,1,0,0,0,0],
            [0,0,0,0,0,0,1,0,0,0],
            [0,0,0,0,0,0,0,1,0,0],
            [0,0,0,0,0,0,0,0,1,0],
            [0,0,0,0,0,0,0,0,0,1]
        ]

spec :: Spec
spec = do
  describe "pythagoreanTriples" $ do
    it "contains trivial starts" $
      take 5 pythagoreanTriples `shouldBe` [(3,4,5),(6,8,10),(5,12,13),(9,12,15),(8,15,17)]
    it "contains non-trivial triples" $ do
      take 100 pythagoreanTriples `shouldContain` [(25,60,65)]
      take 100 pythagoreanTriples `shouldContain` [(27,36,45)]
      take 100 pythagoreanTriples `shouldContain` [(45,60,75)]
      take 100 pythagoreanTriples `shouldContain` [(40,75,85)]

  describe "eyeMatrix" $ do
    it "works for trivial cases" $ do
      eyeMatrix 0 `shouldBe` [[]]
      eyeMatrix 1 `shouldBe` [[1]]
    it "works for n > 0" $ do
      eyeMatrix 2 `shouldBe` [[1,0],[0,1]]
      eyeMatrix 3 `shouldBe` [[1,0,0],[0,1,0],[0,0,1]]
      eyeMatrix 10 `shouldBe` eye10
      head (eyeMatrix 100) `shouldBe` 1 : replicate 99 0
      last (eyeMatrix 100) `shouldBe` replicate 99 0 ++ [1]

  describe "matrixMultiplication" $ do
    it "works for trivial cases" $ do
      matrixMultiplication [[1]] [[1]] `shouldBe` [[1]]
      matrixMultiplication [[1]] [[2]] `shouldBe` [[2]]
      matrixMultiplication [[10]] [[0]] `shouldBe` [[0]]
    it "works for identity matrix" $ do
      matrixMultiplication matrixA [[1,0],[0,1]] `shouldBe` matrixA
      matrixMultiplication matrixB [[1,0],[0,1]] `shouldBe` matrixB
      matrixMultiplication [[1,0],[0,1]] matrixA `shouldBe` matrixA
      matrixMultiplication [[1,0],[0,1]] matrixB `shouldBe` matrixB
    it "works with square matrices" $ do
      matrixMultiplication matrixA matrixB `shouldBe` [[7,19],[19,43]]
      matrixMultiplication matrixB matrixA `shouldBe` [[20,30],[22,30]]
    it "works with correct sizes" $ do
      matrixMultiplication matrixA matrixC `shouldBe` [[17,5,4],[37,11,10]]
      matrixMultiplication matrixB matrixC `shouldBe` [[50,15,15],[52,15,9]]
      matrixMultiplication matrixC matrixD `shouldBe` [[13,10],[26,12]]
    it "raises error for bad sizes" $ do
      evaluate (matrixMultiplication [[1]] matrixA) `shouldThrow` errorCall "Incorrect matrix sizes"
      evaluate (matrixMultiplication matrixA eye10) `shouldThrow` errorCall "Incorrect matrix sizes"
      evaluate (matrixMultiplication matrixE matrixD) `shouldThrow` errorCall "Incorrect matrix sizes"
