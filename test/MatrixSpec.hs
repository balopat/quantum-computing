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
        let m1 = [[ Cartesian 1 2, Cartesian 2 3]
                  ,[ Cartesian 1 2, Cartesian 2 3]]
            m2 = [[ Cartesian 1 1, Cartesian 2 2 ],
                  [ Cartesian 1 1, Cartesian 2 2 ]] 
        (M.add m1 m2) `shouldBe` [[ Cartesian 2 3, Cartesian 4 5 ],
                                  [ Cartesian 2 3, Cartesian 4 5 ]]
      it "sum of empty matrices is empty matrix" $ do
          (M.add [] []) `shouldBe` []
      it "sum of matrices with different sizes results in error" $ do
          evaluate(M.add [] [[Cartesian 1 1]]) `shouldThrow` errorCall "Sizes are not equal"
    describe "Matrix.inv (inverse):" $ do
      it "should negate a matrix's elements" $ do
        (M.inv [[ Cartesian 1 2, Cartesian (-2) 3]]) `shouldBe` [[ Cartesian (-1) (-2), Cartesian 2 (-3)]]
      it "inverse of empty matrix is empty matrix" $ do
          (M.inv []) `shouldBe` []
    describe "Matrix.scalar:" $ do
      it "should multiply a matrix's elements" $ do
        (M.scalar (Cartesian (-3) 0) [[ Cartesian 1 2, Cartesian (-2) 3]]) `shouldBe` [[ Cartesian (-3) (-6), Cartesian 6 (-9)]]
      it "product of any scalar with empty vector is empty vector" $ do
          (M.scalar (Cartesian 100 1) []) `shouldBe` []


main::IO()
main = do
  hspec spec
