module Ex13 

import Data.Vect

myReverse : Vect n elem -> Vect n elem
myReverse [] = []
myReverse {n = S k} (x :: xs) = rewrite plusCommutative 1 k in myReverse xs ++ [x]

myReverse2 : Vect n elem -> Vect n elem
myReverse2 [] = []
myReverse2 (x :: xs) = reverseProof $ myReverse2 xs ++ [x]
  where reverseProof : Vect (len + 1) elem -> Vect (S len) elem
        reverseProof {len} xs = rewrite plusCommutative 1 len in xs

append_nil : (ys : Vect m elem) -> Vect (plus m 0) elem
append_nil {m} ys = rewrite plusZeroRightNeutral m in ys

append_xs : Vect (S (m + len)) elem -> Vect (plus m (S len)) elem
append_xs {m} {len} xs = rewrite sym $ plusSuccRightSucc m len in xs

append : Vect n elem -> Vect m elem -> Vect (m + n) elem
append [] ys = append_nil ys 
append (x :: xs) ys = append_xs $ x :: append xs ys

------ TODO p. 227

myPlusCommutes : (n : Nat) -> (m : Nat) -> n + m = m + n
myPlusCommutes Z m = rewrite sym $ plusZeroRightNeutral m in Refl
myPlusCommutes (S k) m = ?h

------ TODO



