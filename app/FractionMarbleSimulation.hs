{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, DeriveGeneric, DeriveAnyClass #-}

module Main where

import Control.Applicative ((<$>))
import Data.List.Split
import Complex
import qualified Matrix as M
import Data.Ratio

readInt :: String -> Int
readInt x = read x

readRatio :: String -> Ratio Int
readRatio x = read x

getLines :: Int -> IO [String] 
getLines 0 = return []
getLines n 
  | n < 0 =  do 
    return [] 
  | otherwise = do 
    nextLine <- getLine
    linesAfter <- getLines (n-1) 
    return ([nextLine] ++ linesAfter)

validateDoublyStocahstic :: M.Matrix (Ratio Int) -> Bool
validateDoublyStocahstic m@(M.Matrix _ _ _) = all (\row -> 1 == (sum row)) (M.mx (M.transpose m))

main :: IO ()
main = do
  print "Enter the number of states (n): "
    
  n <- read <$> getLine
  print $ "Enter the state transition matrix (" ++ show(n) ++ "x" ++ show(n) ++"):"
  mx  <- M.matrix . map (\line -> map (readRatio) (splitOn "," line)) <$> (getLines n)
  print mx
  
  if (validateDoublyStocahstic mx) then print "Matrix is valid!" else error "Matrix is invalid, not all column sums are 1"
    
  print $ "Enter the number of marbles in each state ("++show(n)++" states):"
  
  state <- map (readRatio) <$> (getLines n)
  
  print "Initial state:" 
  print $ show(state)

  print "How many clicks?" 
  clicks <- read <$> getLine 
  
  print $ "M^" ++ show(clicks) ++": "
  let pow = (iterate (M.mul mx) mx) !! (clicks -1)
  print  pow
  
  print "and the final state:" 
  
  print (M.vmul pow state) 
  