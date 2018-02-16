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


not_in_nil : DecEq a => (x : a) -> Elem v xs -> Void
not_in_nil x y = ?not_in_nil_rhs

isElem : DecEq a => (x : a) -> (Vect n a) -> Dec $ Elem v xs 
isElem x [] = No (not_in_nil x)
isElem x (y :: xs) = ?isElem_rhs_2
 
