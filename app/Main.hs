{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Control.Monad
import Happstack.Server ( Method(GET,POST), method, Browsing(EnableBrowsing), nullConf
                        , serveDirectory, simpleHTTP, serveFile, toResponse, asContentType, ok, dir, path, uriRest
                        )
import Debug.Trace
import Text.JSON.Generic
import Complex


main :: IO ()
main = simpleHTTP nullConf $
 msum [ dir "static" $ uriRest $ \s -> serveFile (asContentType "text/html") ("web/static/" ++ (trace ("value of s is: " ++ s) s) )
        ,dir "complex" $ ok $ toResponse $ encodeJSON $ (Cartesian 1 1)
        ,do method GET
            serveFile (asContentType "text/html") "web/index.html"
      ]
