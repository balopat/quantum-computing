module CalculationServices where

import Complex
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy.Encoding as T

decodeMulReq :: L.ByteString -> Either String MultiplicationRequest
decodeMulReq body = eitherDecode body :: Either String MultiplicationRequest

evalMulReq :: Either String MultiplicationRequest -> String
evalMulReq (Right mulReq) = L.unpack $ encode $ toJSON (eval mulReq)
evalMulReq (Left err)  = err

handleMultiplicationRequest :: L.ByteString ->  String
handleMultiplicationRequest body = evalMulReq (decodeMulReq body)
