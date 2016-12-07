module Vector where
import Complex

add:: [Complex] -> [Complex] -> [Complex]
add [] [] = []
add a b = if length a /= length b then
              error "Length are not equal"
          else zipWith (<+>) a b
