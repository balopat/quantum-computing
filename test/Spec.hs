

--- | The following json should be parsed as MultiplicationRequest
----- {"points":[{"x":22.8,"y":5.3},{"x":17.4,"y":-6.7},{"x":-11,"y":-5.4}],"mulBy":{"r":"3","i":"3"}}
import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Data.Maybe
import qualified Data.Aeson as A
import Complex
import qualified Data.ByteString.Lazy.Char8 as L

main :: IO ()
main = hspec $ do
  describe "MultiplicationRequest" $ do
    it "should parse from JSON" $ do
      let jsonString = L.pack "{\"points\":[{\"x\":22.8,\"y\":5.3},{\"x\":17.4,\"y\":-6.7}],\"mulBy\":{\"r\":\"3\",\"i\":\"3\"}}"
      A.eitherDecode jsonString `shouldBe` Right (MultiplicationRequest [Cartesian 22.8 5.3,
                                                            Cartesian 17.4 (-6.7)
                                                              ]
                                                              (Cartesian 3 3))

    it "returns the first element of an *arbitrary* list" $
      property $ \x xs -> head (x:xs) == (x :: Int)

    it "throws an exception if used with an empty list" $ do
      evaluate (head []) `shouldThrow` anyException
