module Ex6

import Data.Vect

%default total

Matrix : Nat -> Nat -> Type
Matrix n z = Vect n $ Vect z Double

testMatrix : Matrix 2 3
testMatrix = [[0,0,0],[0,0,0]]

testMatrix2 : Matrix 0 0
testMatrix2 = []

