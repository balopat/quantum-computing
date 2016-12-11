module Vector where
import Complex

add:: [Complex] -> [Complex] -> [Complex]
add [] [] = []
add a b = if length a /= length b then
              error "Length are not equal"
          else zipWith (<+>) a b

inv:: [Complex] -> [Complex]
inv [] = []
inv xs = [Cartesian (-a) (-b) | (Cartesian a b) <- xs]

scalar:: Complex -> [Complex] -> [Complex]
scalar _ [] = []
scalar s xs = [ s |*| x | x <- xs]



inner:: [Complex] -> [Complex] -> Complex
inner [] [] = Cartesian 0 0  
