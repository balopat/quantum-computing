{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, DeriveGeneric, DeriveAnyClass #-}

module Main where

import Control.Monad
import Happstack.Server
import Debug.Trace
import Text.JSON.Generic
import Complex
import CalculationServices
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

complexRoute :: ServerPart Response
complexRoute = do
      body <- getBody
      ok $ toResponse $ handleMultiplicationRequest body

main :: IO ()
main = simpleHTTP nullConf $ do decodeBody myPolicy
                                msum [
                                   dir "static" $ uriRest $ \s -> serveFile (asContentType "text/html") ("web/static/" ++ trace ("value of s is: " ++ s) s)
                                  ,dir "complex" complexRoute
                                  ,do method GET
                                      serveFile (asContentType "text/html") "web/index.html"
                                      ]
