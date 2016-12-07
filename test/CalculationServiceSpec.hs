module CalculationServiceSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Data.Maybe
import qualified Data.Aeson as A
import Complex
import CalculationService
import qualified Data.ByteString.Lazy.Char8 as L


main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "CalculationService" $ do
    it "should parse Complex from JSON" $ do
        let jsonString = L.pack "{\"r\":22.8,\"i\":5.3}"
        A.eitherDecode jsonString `shouldBe` Right (Cartesian 22.8 5.3)

    it "should parse CalculationRequest from JSON" $ do
      let jsonString = L.pack "{\"points\":[{\"r\":22.8,\"i\":5.3},{\"r\":17.4,\"i\":-6.7}],\"operator\":\"*\", \"operand\":{\"r\":3,\"i\":3}}"
      A.eitherDecode jsonString `shouldBe` Right
                                              (CalculationRequest [
                                                            Cartesian 22.8 5.3
                                                            ,Cartesian 17.4 (-6.7)
                                                            ]
                                                            "*"
                                                            (Cartesian 3 3))
    it "should multiply all numbers and return the array of them" $ do
      let mulReq = CalculationRequest [
                    Cartesian 3.0 4.0
                    ,Cartesian 4.0 5.0
                    ]
                    "*"
                    (Cartesian 3 3)
      calculate mulReq `shouldBe` [Cartesian (-3.0) 21, Cartesian (-3.0) 27 ]

    it "should divide all numbers and return the array of them" $ do
        let mulReq = CalculationRequest [
                      Cartesian 1.0 2.0
                      ,Cartesian 3.0 2.0
                      ]
                      "/"
                      (Cartesian 1 2)
        calculate mulReq `shouldBe` [Cartesian 1 0, Cartesian 1.4 (-0.8) ]
