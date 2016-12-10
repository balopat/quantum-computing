module MatrixSpec(main, spec) where

import Test.Hspec
import qualified Matrix as M
import Complex
import Control.Exception

spec::Spec
spec = do
  describe "Complex Matrix operations:" $ do
    describe "Matrix.add:" $ do
      it "should add two matrices element by element" $ do
        let m1 = (M.matrix [[ Cartesian 1 2, Cartesian 2 3]
                  ,[ Cartesian 1 2, Cartesian 2 3]])
            m2 = (M.matrix [[ Cartesian 1 1, Cartesian 2 2 ],
                  [ Cartesian 1 1, Cartesian 2 2 ]])
            expected = (M.matrix [[ Cartesian 2 3, Cartesian 4 5 ],
                                      [ Cartesian 2 3, Cartesian 4 5 ]])
        (M.add m1 m2) `shouldBe` expected
      it "sum of empty matrices is empty matrix" $ do
          (M.add (M.matrix [])  (M.matrix []) ) `shouldBe` (M.matrix [])
      it "sum of matrices with different sizes results in error" $ do
          evaluate(M.add (M.matrix []) (M.matrix [[Cartesian 1 1]])) `shouldThrow` errorCall "Sizes are not equal: 0x0 vs 1x1"
    describe "Matrix.inv (inverse):" $ do
      it "should negate a matrix's elements" $ do
        (M.inv (M.matrix [[ Cartesian 1 2, Cartesian (-2) 3]])) `shouldBe` M.matrix [[ Cartesian (-1) (-2), Cartesian 2 (-3)]]
      it "inverse of empty matrix is empty matrix" $ do
          M.inv (M.matrix []) `shouldBe` M.matrix []
    describe "Matrix.scalar:" $ do
      it "should multiply a matrix's elements" $ do
        (M.scalar (Cartesian (-3) 0) (M.matrix [[ Cartesian 1 2, Cartesian (-2) 3]])) `shouldBe` (M.matrix [[ Cartesian (-3) (-6), Cartesian 6 (-9)]])
      it "product of any scalar with empty vector is empty matrix" $ do
          M.scalar  (Cartesian 100 1) (M.matrix []) `shouldBe` M.matrix []
    describe "Matrix.mul:" $ do
      it "the product of empty and empty is empty" $ do
        M.mul (M.matrix [])  (M.matrix []) `shouldBe` (M.matrix [])


main::IO()
main = do
  hspec spec
