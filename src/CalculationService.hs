{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveAnyClass #-}

module CalculationService where

import Complex
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy.Encoding as T
import Data.Data (Data, Typeable)
import GHC.Generics
import Data.Aeson


instance FromJSON Complex where
  parseJSON = withObject "Complex" $ \o -> do
    r <- o.: "r"
    i <- o.: "i"
    return (Cartesian r i)

instance ToJSON Complex where
  toJSON (Cartesian r i) = object [
      "r" .= r,
      "i"  .= i]


data CalculationRequest = CalculationRequest {
    points :: [Complex],
    operator :: String,
    operand :: Complex
} deriving (Show, Eq, Generic, ToJSON)

instance FromJSON CalculationRequest where
  parseJSON = withObject "CalculationRequest" $ \o -> do
    points <- o .: "points"
    operator <- o .: "operator"
    operand  <- o .: "operand"
    return (CalculationRequest points operator operand)

calculate :: CalculationRequest -> [Complex]
calculate (CalculationRequest points "*" operand) = map (* operand) points
calculate (CalculationRequest points "/" operand) = map (</> operand) points

decodeCalculationRequest :: L.ByteString -> Either String CalculationRequest
decodeCalculationRequest body = eitherDecode body :: Either String CalculationRequest

calculateRequest :: Either String CalculationRequest -> String
calculateRequest (Right mulReq) = L.unpack $ encode $ toJSON (calculate mulReq)
calculateRequest (Left err)  = err

handleCalculationRequest :: L.ByteString ->  String
handleCalculationRequest body = calculateRequest (decodeCalculationRequest body)


----- |Division Service
------------------------------
