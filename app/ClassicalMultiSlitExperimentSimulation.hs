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

-- reads from the user the n x m matrix for each slit the probabilities for a bullet to end up in target t0-tm
getProbabilities :: Int -> Int -> Int -> IO [[Ratio Int]]
getProbabilities nSlots slot nTargets 
  | slot == nSlots = do 
    return []
  | otherwise = do 
    print $ "For slit #" ++ show(slot) 
    slitProbabilities <- map (readRatio) <$> getLines nTargets 
    slitProbabilitiesAFter <- getProbabilities nSlots (slot + 1) nTargets
    return ([slitProbabilities] ++ slitProbabilitiesAFter)

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
  print "Enter the number of slits (2 in the two-slit): "
    
  s <- read <$> getLine

  print "Enter the number of targets to measure the bullets (5 in the book): "
    
  d <- read <$> getLine

  print "Enter the slit -> target probabilities: " 
  
  probs <- getProbabilities s 0 d
  
  print $ show(probs)
  
  let n =  1 + s + d
   
  let mxGen = (\i j ->  if (i == 0) then 0%1            -- no bullet goes/stays at the start point (gun)
                        else if (i <= s && i > 0) then 
                          if (j == 0) then 1%s else 0%1 -- equal probability to each slot from gun 
                        else if (i > s && j == 0) then  0%1
                        else if (i > s && j <= s && j > 0) then                         
                            probs !! (j-1) !! (i-s-1)  -- probabilities to each device from the slits 
                        else -- if (i > s && j > s) then                           
                          if ( i == j) then (1%1) else (0%1) -- once a particle arrived to a device, it stays there                            
              )
  
  let mx = M.matrix [[ mxGen i j | j <- [0..(n-1)]] | i <-[0..(n-1)]]
    
  print $ mx
  
  let state = [1%1] ++ (take (n-1) (repeat 0)) 
  
  print "Initial state:" 
  print $ show(state)
  
  let clicks = 2
  
  print $ "M^" ++ show(clicks) ++": "
  let pow = (iterate (M.mul mx) mx) !! (clicks -1)
  print  pow
  
  print "and the final state:" 
  
  print (M.vmul pow state) 
  