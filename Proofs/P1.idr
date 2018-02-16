module P1 

import Data.Vect

%default total

data EqNat : (n1 : Nat) -> (n2 : Nat) -> Type where
  Same : (num : Nat) -> EqNat num num

checkEqNat : (num1 : Nat) -> (num2 : Nat) -> Maybe $ EqNat num1 num2
checkEqNat Z Z = Just $ Same _
checkEqNat Z (S k) = Nothing
checkEqNat (S k) Z = Nothing
checkEqNat (S k) (S j) = case checkEqNat k j of
                              Nothing => Nothing
                              Just $ Same j => Just $ Same $ S _
                              
exactLength' : (len : Nat) -> (input : Vect m a) -> Maybe $ Vect len a 
exactLength' {m} len input = case checkEqNat m len of
                                  Nothing => Nothing
                                  Just $ Same len => Just $ input
                                  
{-

λΠ> the (2 + 2 = 4) Refl
Refl : 4 = 4

-}

checkEqNat' : (num1 : Nat) -> (num2 : Nat) -> Maybe (num1 = num2)
checkEqNat' Z Z = Just Refl
checkEqNat' Z (S k) = Nothing
checkEqNat' (S k) Z = Nothing
checkEqNat' (S k) (S j) = case checkEqNat' k j of
                               Nothing => Nothing
                               (Just prf) => Just $ cong prf

cong' : {f : a -> b} -> x = y -> f x = f y
cong' Refl = Refl

rotate : Vect n a -> Vect n a
rotate [] = []
rotate (x :: xs) = rotateProof $ xs ++ [x]
  where rotateProof : Vect (len + 1) a -> Vect (S len) a
        rotateProof {len} xs = rewrite plusCommutative 1 len in xs

plus_commutes_Z : (n : Nat) -> plus Z n = plus n Z
plus_commutes_Z Z = Refl
plus_commutes_Z (S k) = rewrite plus_commutes_Z k in Refl

plus_commutes_S : (k : Nat) -> (n : Nat) -> S (n + k) = n + (S k) 
plus_commutes_S k Z = Refl
plus_commutes_S k (S j) = rewrite plus_commutes_S k j in Refl

plus_commutes : (m, n : Nat) -> m + n = n + m
plus_commutes Z n = plus_commutes_Z n
plus_commutes (S k) n = rewrite plus_commutes k n in plus_commutes_S k n

prop : (n, k : Nat) -> n + S k = S (n + k)
prop n k = sym $ plusSuccRightSucc n k

not2eq3 : 2 = 3 -> Void
not2eq3 Refl impossible

notSZ : (S k = 0) -> Void
notSZ Refl impossible

notSS : (contra : (k = j) -> Void) -> (S k = S j) -> Void
notSS contra Refl = contra Refl

notZS : (0 = S k) -> Void
notZS Refl impossible

checkEqNat'' : (num1 : Nat) -> (num2 : Nat) -> Dec (num1 = num2)
checkEqNat'' Z Z = Yes Refl
checkEqNat'' Z (S k) = No notZS
checkEqNat'' (S k) Z = No notSZ
checkEqNat'' (S k) (S j) = case checkEqNat'' k j of
                                (Yes Refl) => Yes Refl
                                (No contra) => No (notSS contra)

exactLength2 : (len : Nat) -> (input : Vect m a) -> Maybe $ Vect len a
exactLength2 {m} len input = case decEq m len of
                                  (Yes Refl) => Just input
                                  (No contra) => Nothing

removeDecElem : DecEq a => a -> Vect (S n) a -> Vect n a
removeDecElem x xs = ?removeDecElem_rhs
