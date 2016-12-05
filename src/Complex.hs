{-# LANGUAGE OverloadedStrings, DeriveAnyClass #-}

module Complex where

import Data.List.Split
import GHC.Generics


data Complex =  Cartesian { r:: Double, i::Double } |
                Polar { rho :: Double, theta::Double}
                  deriving (Show,Eq)

readComplex :: String -> Complex
readComplex s = let parts = splitOn " " s
          in Cartesian (read (head parts)) (read (parts!!1))

(<+>) :: Complex -> Complex -> Complex
(Cartesian r1 i1) <+>  (Cartesian r2 i2) = Cartesian (r1 + r2) (i1 + i2)

(<->) :: Complex -> Complex -> Complex
(Cartesian r1 i1) <->  (Cartesian r2 i2) = Cartesian (r1 - r2) (i1 - i2)

(|*|) :: Complex -> Complex -> Complex
(Cartesian r1 i1) |*| (Cartesian r2 i2) = Cartesian (r1 * r2 - i1 * i2) (r1 * i2 + r2 * i1)
(Polar rho1 theta1) |*| (Polar rho2 theta2) = Polar (rho1 * rho2) (theta1 + theta2)

(</>) :: Complex -> Complex -> Complex
(Cartesian r1 i1) </>  (Cartesian r2 i2) = Cartesian ((r1 * r2 + i1 * i2)/divisor) ((r2 * i1 - r1 * i2)/divisor)
                                       where divisor = r2*r2+i2*i2
(Polar rho1 theta1) </>  (Polar rho2 theta2) = Polar (rho1/rho2) (theta1 -theta2)

conj :: Complex -> Complex
conj (Cartesian r i) = Cartesian r (-i)

modulus :: Complex -> Double
modulus (Cartesian r i) = sqrt $ r*r + i*i

toPolar :: Complex -> Complex
toPolar (Polar rho theta) = Polar rho theta
toPolar (Cartesian r i)   = Polar rho theta
                           where rho = modulus $ Cartesian r i
                                 theta = atan $ i/r

toCartesian :: Complex -> Complex
toCartesian (Polar rho theta) = Cartesian r i
                                where r = rho * cos theta
                                      i = rho * sin theta
toCartesian (Cartesian r i)   = Cartesian r i


showPi :: Complex -> String
showPi (Polar rho theta) = show(rho) ++ ", " ++ show(theta/pi) ++ "pi"
