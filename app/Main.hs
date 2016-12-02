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

import Data.Maybe
import Data.Aeson

getBody :: ServerPart L.ByteString
getBody = do rq <- askRq
             bdy <- liftIO (readMVar (rqBody rq))
             return (unBody bdy)

myPolicy :: BodyPolicy
myPolicy = defaultBodyPolicy "/tmp/" 0 1000 1000

respondTo :: L.ByteString -> Complex
respondTo body =
  let uBody = L.unpack body
      grrr = show(eitherDecode body :: Either String MultiplicationRequest)
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
