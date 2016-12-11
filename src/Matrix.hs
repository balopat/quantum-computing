module Matrix (matrix, add, inv, scalar, Matrix, mul, transpose) where
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
          else Cnm (zipWith V.add a b) n1 m1

inv:: Matrix -> Matrix
inv (Cnm [] 0 0) = Cnm [] 0 0
inv (Cnm rows _ _) = matrix [[ Cartesian (-a) (-b) | (Cartesian a b) <- row] | row <- rows]

scalar:: Complex -> Matrix -> Matrix
scalar _ (Cnm [] 0 0) = Cnm [] 0 0
scalar s (Cnm rows _ _) = matrix [[ s |*| x | x <- row] | row <- rows]

transpose:: Matrix -> Matrix
transpose (Cnm [] 0 0) = Cnm [] 0 0
transpose (Cnm a n m) = matrix [[ a !! i !! j | i <- [0..n-1]] | j <- [0..m-1]]

mul:: Matrix -> Matrix -> Matrix
mul (Cnm [] 0 0) (Cnm [] 0 0) = (Cnm [] 0 0)
mul (Cnm  a n1 m1) mb@(Cnm b n2 m2) = if (m1 == n2) then
       let trB = mx (transpose mb) in
       (matrix [[ Complex.sum (zipWith (|*|) (a !! j) (trB !! i))  | i <- [0..m2-1]] | j <- [0..n1-1]])
    else
      error  ("Matrices are not compatible for multiplication: (" ++ (show n1) ++ "x" ++ (show m1) ++ ") * ("  ++ (show n2) ++ "x" ++ (show m2) ++ ")")
