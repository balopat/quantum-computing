

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
main = hspec $
  describe "Complex" $ do
    it "should parse MultiplicationRequest from JSON" $ do
      let jsonString = L.pack "{\"points\":[{\"r\":22.8,\"i\":5.3},{\"r\":17.4,\"i\":-6.7}],\"mulBy\":{\"r\":3,\"i\":3}}"
      A.eitherDecode jsonString `shouldBe` Right
                                              (MultiplicationRequest [
                                                            Cartesian 22.8 5.3
                                                            ,Cartesian 17.4 (-6.7)
                                                            ]
                                                            (Cartesian 3 3))


    it "should parse Complex from JSON" $ do
      let jsonString = L.pack "{\"r\":22.8,\"i\":5.3}"
      A.eitherDecode jsonString `shouldBe` Right (Cartesian 22.8 5.3)


    it "should multiply all numbers and return the array of them" $ do
      let mulReq = MultiplicationRequest [
                    Cartesian 3.0 4.0
                    ,Cartesian 4.0 5.0
                    ]
                    (Cartesian 3 3)
      eval mulReq `shouldBe` [Cartesian (-3.0) 21, Cartesian (-3.0) 27 ]
