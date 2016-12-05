{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveAnyClass #-}

module CalculationServices where

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


data MultiplicationRequest = MultiplicationRequest {
    points :: [Complex],
    mulBy :: Complex
} deriving (Show, Eq, Generic, ToJSON)

instance FromJSON MultiplicationRequest where
  parseJSON = withObject "MultiplicationRequest" $ \o -> do
    points <- o .: "points"
    mulBy  <- o .: "mulBy"
    return (MultiplicationRequest points mulBy)

eval :: MultiplicationRequest -> [Complex]
eval (MultiplicationRequest points mulBy) = map (|*| mulBy) points

decodeMulReq :: L.ByteString -> Either String MultiplicationRequest
decodeMulReq body = eitherDecode body :: Either String MultiplicationRequest

evalMulReq :: Either String MultiplicationRequest -> String
evalMulReq (Right mulReq) = L.unpack $ encode $ toJSON (eval mulReq)
evalMulReq (Left err)  = err

handleMultiplicationRequest :: L.ByteString ->  String
handleMultiplicationRequest body = evalMulReq (decodeMulReq body)
