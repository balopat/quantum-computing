module Vector where
import Complex

add:: (Num t) => [t] -> [t] -> [t]
add [] [] = []
add a b = if length a /= length b then
              error "Length are not equal"
          else zipWith (+) a b

inv:: (Num t) => [t] -> [t]
inv [] = []
inv xs = [(-x) | x <- xs]

scalar:: (Num t) =>  t -> [t] -> [t]
scalar _ [] = []
scalar s xs = [ s * x | x <- xs]

conj :: [Complex] -> [Complex]
conj v = (map Complex.conj v)

inner:: [Complex] -> [Complex] -> Complex
inner [] [] = Cartesian 0 0
inner v1 v2 = if (length v1 /= length v2) then
                error ("Length are not equal for inner product: " ++ show(length v1) ++ " vs " ++ show(length v2))
              else
                sum $ zipWith (*) (Vector.conj v1) v2

norm :: [Complex] -> Double
norm [] = 0
norm v = sqrt (r (inner v v))

dist :: [Complex] -> [Complex] -> Double
dist v1 v2 = norm $ add v1 (Vector.inv v2)
