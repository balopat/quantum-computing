module Matrix where
import Complex
import qualified Vector as V
import  Control.Exception

data Matrix a = Matrix  {
    mx :: [[a]],
    n :: Int,
    m :: Int
} deriving (Eq)

showVec :: (Show a) => [a] -> [Char]
showVec nums = (foldl (\acc next -> acc ++ show(next) ++ "\t")  "" nums)

instance (Show a) => Show (Matrix a) where
  show (Matrix mx n m) = "Matrix: " ++ show(n) ++ "x" ++ show(m) ++ "\n" ++ (foldl (\acc next -> acc ++ (showVec next) ++ "\n") "" mx)

matrix :: [[t]] -> (Matrix t)
matrix [] = Matrix [] 0 0
matrix rows = let n = length rows
                  m = length (head rows) in
              assert (all (\row -> length row == m) rows) (Matrix rows n m)

add:: (Num t) => Matrix t -> Matrix t -> Matrix t
add (Matrix [] 0 0) (Matrix [] 0 0) = Matrix [] 0 0
add (Matrix a n1 m1) (Matrix b n2 m2) = if (n1 /= n2) || (m1 /= m2) then
            error ("Sizes are not equal: " ++ (show n1) ++ "x" ++ (show m1) ++ " vs "  ++ (show n2) ++ "x" ++ (show m2))
          else Matrix (zipWith V.add a b) n1 m1

inv:: (Num t) => Matrix t -> Matrix t
inv (Matrix [] 0 0) = Matrix [] 0 0
inv (Matrix rows _ _) = matrix [[ (-x) | x <- row] | row <- rows]

scalar:: (Num t) => t -> Matrix t -> Matrix t
scalar _ (Matrix [] 0 0) = Matrix [] 0 0
scalar s (Matrix rows _ _) = matrix [[ s * x | x <- row] | row <- rows]

transpose:: Matrix t -> Matrix t
transpose (Matrix [] 0 0) = Matrix [] 0 0
transpose (Matrix a n m) = matrix [[ a !! i !! j | i <- [0..n-1]] | j <- [0..m-1]]

mul:: (Num t) => Matrix t -> Matrix t -> Matrix t
mul (Matrix [] 0 0) (Matrix [] 0 0) = (Matrix [] 0 0)
mul (Matrix  a n1 m1) mb@(Matrix b n2 m2) = if (m1 == n2) then
       let trB = mx (transpose mb) in
       (matrix [[ sum (zipWith (*) (a !! j) (trB !! i))  | i <- [0..m2-1]] | j <- [0..n1-1]])
    else
      error  ("Matrices are not compatible for multiplication: (" ++ (show n1) ++ "x" ++ (show m1) ++ ") * ("  ++ (show n2) ++ "x" ++ (show m2) ++ ")")

vmul :: (Num t) => Matrix t -> [t] -> [t]
vmul a v = head $ mx $ transpose $ mul a (transpose (matrix [v]))


identity :: (Num t) => Int -> Matrix t
identity 0 = matrix []
identity n = matrix [[ if (i == j) then 1 else 0 | j <- [0..n-1]] | i <- [0..n-1]]

mconj :: Matrix Complex -> Matrix Complex
mconj m@(Matrix _ _ _) = m
mconj (Matrix rows n m) =  matrix [ V.conj row | row <- rows ]

adj :: Matrix Complex -> Matrix Complex 
adj (Matrix [] 0 0) = (Matrix [] 0 0)
adj m@(Matrix _ _ _) = mconj $ transpose m
adj m@(Matrix _ _ _) = transpose m

isHermitian :: Matrix Complex -> Bool
isHermitian (Matrix [] 0 0) = True
isHermitian (Matrix mx n m) = n == m &&  all (\(i,j) -> mx !! i !! j == (Complex.conj ( mx !! j !! i))) [ (i,j) | i <- [0..n-1], j <- [0..i]]

isUnitary :: Matrix Complex -> Bool
isUnitary (Matrix [] 0 0) = True
isUnitary mx@(Matrix _ n m) = n == m &&  ( (mul (Matrix.adj mx) mx) == identity n )

tensor :: (Num t) => Matrix t -> Matrix t -> Matrix t
tensor (Matrix [] 0 0) (Matrix [] 0 0) = (Matrix [] 0 0)
tensor (Matrix a m m_) (Matrix b n n_) = matrix [[ (a !! (j `div` n) !! (k `div` m)) * (b !! (j `mod` n) !! (k `mod` m)) | k <- [0..(m_ * n_ -1)] ] | j <- [0..(m*n-1)] ]
