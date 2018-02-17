module P2

import Data.Vect

removeElem : DecEq a => (v : a) -> (xs : Vect (S n) a) -> Elem v xs -> Vect n a
removeElem x (x :: xs) Here = xs
removeElem {n = Z} _ _ (There _) impossible
removeElem {n = (S k)} v (x :: xs) (There later) = x :: removeElem v xs later

removeElem_auto : DecEq a => (v : a) -> (xs : Vect (S n) a) -> {auto prf : Elem v xs} -> Vect n a
removeElem_auto v xs {prf} = removeElem v xs prf

{-
λΠ> removeElem 3 [1,3,2] (There Here)
[1, 2] : Vect 2 Integer

λΠ> removeElem_auto 3 [1,2,3,4,5]
[1, 2, 4, 5] : Vect 4 Integer
-}

hasOne : Elem 1 [1,2,3]
hasOne = Here

hasFalse : Elem False [True, False]
hasFalse = There Here

{-
hasZero : Elem 0 [1,2,3]
hasZero = There (There (There ?zero))
-}

not_lte : (j : Nat) -> (k : Nat) -> (contra : Not (LTE (S k) (S j))) -> Not (LTE k j)
not_lte j k contra prf = contra (LTESucc prf)

not_lte__gt : Not (x `LTE` y) -> x `GT` y
not_lte__gt {x = Z} {y} contra = void $ contra LTEZero 
not_lte__gt {x = (S k)} {y = Z} contra = LTESucc LTEZero
not_lte__gt {x = (S k)} {y = (S j)} contra = LTESucc $ not_lte__gt $ not_lte j k contra

data Sorted : (xs : Vect n Nat) -> Type where
  SortedEmpty : Sorted []
  SortedOne : (x : Nat) -> Sorted [x]
  SortedMany : (x : Nat) -> (y : Nat) -> Sorted (y :: zs) -> (x `LTE` y) -> Sorted (x :: y :: zs)
  
sortedVect12 : Sorted [1,2]
sortedVect12 = SortedMany 1 2 (SortedOne 2) (LTESucc LTEZero)

sortedVec012 : Sorted [0,1,2]
sortedVec012 = SortedMany _ _ sortedVect12 LTEZero

