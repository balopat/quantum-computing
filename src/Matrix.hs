module Matrix (matrix, add, inv, scalar, Matrix, mul) where
import Complex
import qualified Vector as V
import  Control.Exception

data Matrix = Cnm {
    mx :: [[Complex]],
    n :: Int,
    m :: Int
} deriving (Show, Eq)

matrix :: [[Complex]] -> Matrix
matrix [] = Cnm [] 0 0
matrix rows = let n = length rows
                  m = length (head rows) in
              assert (all (\row -> length row == m) rows) (Cnm rows n m)


add:: Matrix -> Matrix -> Matrix
add (Cnm [] 0 0) (Cnm [] 0 0) = Cnm [] 0 0
add (Cnm a n1 m1) (Cnm b n2 m2) = if (n1 /= n2) || (m1 /= m2) then
            error ("Sizes are not equal: " ++ (show n1) ++ "x" ++ (show m1) ++ " vs "  ++ (show n2) ++ "x" ++ (show m2))
          else Cnm (zipWith (V.add) a b) n1 m1

inv:: Matrix -> Matrix
inv (Cnm [] 0 0) = Cnm [] 0 0
inv (Cnm rows _ _) = matrix [[ Cartesian (-a) (-b) | (Cartesian a b) <- row] | row <- rows]

scalar:: Complex -> Matrix -> Matrix
scalar _ (Cnm [] 0 0) = (Cnm [] 0 0)
scalar s (Cnm rows _ _) = matrix [[ s |*| x | x <- row] | row <- rows]

mul:: Matrix -> Matrix -> Matrix
mul (Cnm [] 0 0) (Cnm [] 0 0) = (Cnm [] 0 0)
