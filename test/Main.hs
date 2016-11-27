import System.IO
import Complex

main = do
  print "Specify c1 (complex number) in form a b"
  c1 <- fmap readComplex getLine

  print "Specify c2 (complex number) in form a b"
  c2 <- fmap readComplex getLine

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
  print $ "toPolar (with pi) c2: " ++ showPi(toPolar c2)
  print $ "toCartesian toPolar c2: " ++ show(toCartesian(toPolar c2))
  
