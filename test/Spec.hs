

--- | The following json should be parsed as MultiplicationRequest
----- {"points":[{"x":22.8,"y":5.3},{"x":17.4,"y":-6.7},{"x":-11,"y":-5.4}],"mulBy":{"r":"3","i":"3"}}
import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

main :: IO ()
main = hspec $ do
  describe "Prelude.head" $ do
    it "returns the first element of a list" $ do
      head [23 ..] `shouldBe` (23 :: Int)

    it "returns the first element of an *arbitrary* list" $
      property $ \x xs -> head (x:xs) == (x :: Int)

    it "throws an exception if used with an empty list" $ do
      evaluate (head []) `shouldThrow` anyException
