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
      it "inner product of empty vectors should be 0" $ do
        (V.inner [] []) `shouldBe` Cartesian 0 0
      it "inner product of vectors with wrong sizes results in error" $ do
        evaluate(V.inner [Cartesian 1 1] []) `shouldThrow` errorCall "Length are not equal for inner product: 1 vs 0"
      it "inner product of v1 v2 is adj(v1) x v2" $ do
        (V.inner [Cartesian 3 1, Cartesian 0 1] [Cartesian 1 1, Cartesian 1 0]) `shouldBe` Cartesian 4 1

    describe "Vector.norm:" $ do
      it "norm of empty vector should be 0" $ do
        (V.norm []) `shouldBe` 0.0
      it "norm of a vector should be sqrt(<v,v>) " $ do
        (V.norm [Cartesian 0 1, Cartesian 1 0]) `shouldBe` sqrt 2

    describe "Vector.dist:" $ do
      it "distance of v1, v2 should be norm(v2-v1) " $ do
        (V.dist [Cartesian 3 1, Cartesian 1 0, Cartesian 0 0] [Cartesian 2 1, Cartesian 2 (-1), Cartesian 0 1]) `shouldBe` 2.0

main::IO()
main = do
  hspec spec
