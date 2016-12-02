{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, DeriveGeneric, DeriveAnyClass #-}

module Main where

import Control.Monad
import Happstack.Server
import Debug.Trace
import Text.JSON.Generic
import Complex
import GHC.Generics
import Control.Concurrent.MVar (readMVar)

import qualified Data.ByteString.Lazy.Char8 as L
import Happstack.Server.Types
import Control.Monad.IO.Class (liftIO)

import Data.Data (Data, Typeable)
import Data.Maybe
import qualified Data.Aeson as A

data MultiplicationRequest = MultiplicationRequest {
    points :: [Complex],
    mulBy :: Complex
} deriving (Show, Eq, Data, Typeable, Generic, A.ToJSON, A.FromJSON)

-- put this function in a library somewhere
-- getBody :: ServerPart L.ByteString
-- getBody = do
--     req  <- askRq
--     body <- liftIO $ takeRequestBody req
--     case body of
--         Just rqbody -> return . unBody $ rqbody
--         Nothing     -> return "emptystuff"

-- myRoute :: ServerPart Response
-- myRoute = do
--     body <- getBody -- it's a ByteString
--     let unit = fromJust $ A.decode body :: Unit -- how to parse json
--     ok $ toResponse $ A.encode unit
getBody :: ServerPart L.ByteString
getBody = do rq <- askRq
             bdy <- liftIO (readMVar (rqBody rq))
             return (unBody bdy)

myPolicy :: BodyPolicy
myPolicy = defaultBodyPolicy "/tmp/" 0 1000 1000

respondTo :: L.ByteString -> Complex
respondTo body =
  let uBody = L.unpack body
      grrr = show(A.eitherDecode body :: Either String MultiplicationRequest)
      bla = trace grrr uBody in
  Cartesian (fromIntegral (length bla))  1

complexRoute :: ServerPart Response
complexRoute = do
      body <- getBody
      ok $ toResponse $ encodeJSON $ respondTo body

main :: IO ()
main = simpleHTTP nullConf $ do decodeBody myPolicy
                                msum [
                                   dir "static" $ uriRest $ \s -> serveFile (asContentType "text/html") ("web/static/" ++ trace ("value of s is: " ++ s) s)
                                  ,dir "complex" complexRoute
                                  ,do method GET
                                      serveFile (asContentType "text/html") "web/index.html"
                                      ]
