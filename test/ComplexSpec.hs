module ComplexSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Data.Maybe
import qualified Data.Aeson as A
import Complex
import qualified Data.ByteString.Lazy.Char8 as L


main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Complex" $ do
    it "should add tow Cartesian numbers" $ do
        Cartesian 1 1 <+> Cartesian 2 (-3) `shouldBe` Cartesian 3 (-2)
