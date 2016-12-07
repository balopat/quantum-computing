module VectorSpec(main, spec) where

import Test.Hspec
import qualified Vector as V
import Complex
import Control.Exception

spec::Spec
spec = do
  describe "Vector" $ do
    it "should add two vectors" $ do
      (V.add [ Cartesian 1 2, Cartesian 2 3] [ Cartesian 1 1, Cartesian 2 2 ]) `shouldBe` [ Cartesian 2 3, Cartesian 4 5 ]
    it "sum of empty vectors is empty vector" $ do
        (V.add [] []) `shouldBe` []
    it "sum of  vectors with different sizes results in errorr" $ do
        evaluate(V.add [] [Cartesian 1 1]) `shouldThrow` errorCall "Length are not equal"

main::IO()
main = do
  hspec spec
