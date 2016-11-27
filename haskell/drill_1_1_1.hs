import System.IO
import Data.List.Split

data Complex = Complex Float Float deriving (Show)

readComplex :: String -> Complex 
readComplex s = let parts = splitOn " " s 
          in Complex (read (head parts)) (read (parts!!1))

(<+>) :: Complex -> Complex -> Complex
(Complex r1 i1) <+>  (Complex r2 i2) = Complex (r1 + r2) (i1 + i2)

(<->) :: Complex -> Complex -> Complex
(Complex r1 i1) <->  (Complex r2 i2) = Complex (r1 - r2) (i1 - i2)

(|*|) :: Complex -> Complex -> Complex
(Complex r1 i1) |*| (Complex r2 i2) = Complex (r1 * r2 - i1 * i2) (r1 * i2 + r2 * i1)

(</>) :: Complex -> Complex -> Complex
(Complex r1 i1) </>  (Complex r2 i2) = Complex ((r1 * r2 + i1 * i2)/(r2*r2 + i2 * i2)) ((r2 * i1 - r1 * i2)/(r2*r2 + i2 * i2))


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
  
  