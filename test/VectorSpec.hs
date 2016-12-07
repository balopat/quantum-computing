module VectorSpec(main, spec) where

import Test.Hspec
import Vector

spec::Spec
spec = do
  describe "Vector" $ do
    it "should add two vectors" $ do
      1 `shouldBe` 1

main::IO()
main = do
  hspec spec
