module Matrix where
import Complex
import qualified Vector as V

add:: [[Complex]] -> [[Complex]] -> [[Complex]]
add [] [] = []
add a b = if length a /= length b then
              error "Sizes are not equal"
          else zipWith (V.add) a b

inv:: [[Complex]] -> [[Complex]]
inv [] = []
inv rows = [[ Cartesian (-a) (-b) | (Cartesian a b) <- row] | row <- rows]

scalar:: Complex -> [[Complex]] -> [[Complex]]
scalar _ [] = []
scalar s rows = [[ s |*| x | x <- row] | row <- rows]
