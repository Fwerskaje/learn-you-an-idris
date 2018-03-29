module NonZeroVec

data Vect : (n : Nat) -> (elem : Type) -> Type where
  Nil : Vect Z elem
  (::) : (x : elem) -> (xs : Vect len elem) -> Vect (S len) elem

data NonZeroVec : (n : Nat) -> (Not (n = Z)) -> Vect n a -> Type where
  MkNonZeroVec : {n : Nat} -> (prf : (Not (n = Z))) -> (xs : Vect n a) -> NonZeroVec n prf xs


n1 : (prf : Not (n = Z)) -> (xs : Vect n a) -> (n = Z) -> Void
n1 prf [] prf1 = prf Refl
n1 prf (x :: xs) prf1 = prf prf1

nonZeroXs : (prf : Dec (Not (n = Z))) -> (xs : Vect n a) -> Not (n = Z)
nonZeroXs (Yes prf) xs = n1 prf xs
nonZeroXs (No contra) xs = ?n2 

nonZeroVec : {n : Nat} -> (prf : (Dec (Not (n = Z)))) -> 
             (xs : Vect n a) -> Maybe (NonZeroVec n (nonZeroXs prf xs) xs)
nonZeroVec (Yes prf) xs = ?nonZeroVec_rhs_1
nonZeroVec (No _) _ = Nothing
