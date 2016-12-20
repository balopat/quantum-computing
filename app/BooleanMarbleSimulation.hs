{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, DeriveGeneric, DeriveAnyClass #-}

module Main where

import Control.Applicative ((<$>))
import Data.List.Split
import Complex
import qualified Matrix as M

readInt :: String -> Int
readInt x = read x

readBool :: String -> Int
readBool s = if ( s == "1" || s == "0" ) then read s else error "only zeros and ones are allowed!"

getLines :: Int -> IO [String] 
getLines 0 = return []
getLines n 
  | n < 0 =  do 
    return [] 
  | otherwise = do 
    nextLine <- getLine
    linesAfter <- getLines (n-1) 
    return ([nextLine] ++ linesAfter)



main :: IO ()
main = do
  print "Enter the number of states (n): "
    
  n <- read <$> getLine
  print $ "Enter the state transition matrix (" ++ show(n) ++ "x" ++ show(n) ++"):"
  mx  <- M.intMatrix . map (\line -> map (readBool) (splitOn "," line)) <$> (getLines n)
  print mx
  
  print $ "Enter the number of marbles in each state ("++show(n)++" states):"
  
  state <- map (readInt) <$> (getLines n)
  
  print "Initial state:" 
  print $ show(state)

  print "How many clicks?" 
  clicks <- read <$> getLine 
  
  print $ "M^" ++ show(clicks) ++": "
  let pow = (iterate (M.mul mx) mx) !! clicks
  print  pow
  
  print "and the final state:" 
  
  print (M.ivmul pow state) 
  