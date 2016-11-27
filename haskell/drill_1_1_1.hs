import System.IO
import Data.List.Split

data Complex = Cartesian Double Double | Polar Double Double deriving (Show) 

readComplex :: String -> Complex 
readComplex s = let parts = splitOn " " s 
          in Cartesian (read (head parts)) (read (parts!!1))

(<+>) :: Complex -> Complex -> Complex
(Cartesian r1 i1) <+>  (Cartesian r2 i2) = Cartesian (r1 + r2) (i1 + i2)

(<->) :: Complex -> Complex -> Complex
(Cartesian r1 i1) <->  (Cartesian r2 i2) = Cartesian (r1 - r2) (i1 - i2)

(|*|) :: Complex -> Complex -> Complex
(Cartesian r1 i1) |*| (Cartesian r2 i2) = Cartesian (r1 * r2 - i1 * i2) (r1 * i2 + r2 * i1)

(</>) :: Complex -> Complex -> Complex
(Cartesian r1 i1) </>  (Cartesian r2 i2) = Cartesian ((r1 * r2 + i1 * i2)/divisor) ((r2 * i1 - r1 * i2)/divisor)
                                       where divisor = (r2*r2+ i2*i2)
conj :: Complex -> Complex 
conj (Cartesian r i) = Cartesian r (-i)

modulus :: Complex -> Double 
modulus (Cartesian r i) = sqrt $ r*r + i*i

toPolar :: Complex -> Complex 
toPolar (Polar rho theta) = (Polar rho theta)
toPolar (Cartesian r i) = (Polar rho theta) 
                        where rho = modulus $ Cartesian r i
                              theta = atan $ i/r

toCartesian :: Complex -> Complex 
toCartesian (Polar rho theta) = (Cartesian r i) 
                        where r = rho * cos theta
                              i = rho * sin theta 
toCartesian (Cartesian r i) = Cartesian r i

main = do
  print "Specify c1 (complex number) in form a b"   
  c1String <- getLine
  
  print "Specify c2 (complex number) in form a b"   
  c2String <- getLine
  
  let c1 = readComplex c1String  
  let c2 = readComplex c2String 
  
  print $ "c1 + c2: " ++ show(c1 <+> c2)   
  print $ "c1 - c2 : " ++ show(c1 <-> c2) 
  print $ "c2 - c1: " ++ show(c2 <-> c1) 
  print $ "c1 * c2: " ++ show(c1 |*| c2) 
  print $ "c2 * c1 / c1: " ++ show(c2 |*| c1 </> c1) 
  print $ "c2 / c1: " ++ show(c2 </> c1) 
  print $ "modulus c1: " ++ show(modulus c1)
  print $ "modulus c2: " ++ show(modulus c2)
  print $ "conj c1: " ++ show(conj c1)
  print $ "conj c2: " ++ show(conj c2)
  
  print $ "toPolar c2: " ++ show(toPolar c2)
  print $ "toCartesian toPolar c2: " ++ show(toCartesian(toPolar (c2)))
  
  
  