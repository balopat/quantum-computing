module VectorSpec(main, spec) where

import Test.Hspec
import qualified Vector as V
import Complex
import Control.Exception

spec::Spec
spec = do
  describe "Complex vector operations:" $ do
    describe "Vector.add:" $ do
      it "should add two vectors element by element" $ do
        (V.add [ Cartesian 1 2, Cartesian 2 3] [ Cartesian 1 1, Cartesian 2 2 ]) `shouldBe` [ Cartesian 2 3, Cartesian 4 5 ]
      it "sum of empty vectors is empty vector" $ do
          (V.add [] []) `shouldBe` []
      it "sum of  vectors with different sizes results in error" $ do
          evaluate(V.add [] [Cartesian 1 1]) `shouldThrow` errorCall "Length are not equal"
    describe "Vector.inv (inverse):" $ do
      it "should negate a vector's elements" $ do
        (V.inv [ Cartesian 1 2, Cartesian (-2) 3]) `shouldBe` [ Cartesian (-1) (-2), Cartesian 2 (-3)]
      it "inverse of empty vector is empty vector" $ do
          (V.inv []) `shouldBe` []
    describe "Vector.scalar:" $ do
      it "should multiply a vector's elements" $ do
        (V.scalar (Cartesian (-3) 0) [ Cartesian 1 2, Cartesian (-2) 3]) `shouldBe` [ Cartesian (-3) (-6), Cartesian 6 (-9)]
      it "product of any scalar with empty vector is empty vector" $ do
        (V.scalar (Cartesian 100 1) []) `shouldBe` []
    describe "Vector.inner:" $ do
      it "inner prodcut of empty vectors should be empty vector" $ do
        (V.inner [] []) `shouldBe` Cartesian 0 0


main::IO()
main = do
  hspec spec
