module Ex6

import Data.Vect

%default total

Matrix : Nat -> Nat -> Type
Matrix Z Z = Vect 0 $ Vect 0 Double
Matrix Z n = Vect 0 $ Vect n Double 
Matrix n Z = Vect n $ Vect 0 Double
Matrix n z = Vect n $ Vect z Double

testMatrix : Matrix 2 3
testMatrix = [[0,0,0],[0,0,0]]


