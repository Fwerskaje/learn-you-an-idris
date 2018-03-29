module Fib 

%default total

data Fib : (i : Nat) -> (fb : Nat) -> Type where
  FibZero : Fib 0 0
  FibOne  : Fib 1 1
  FibSucc : Fib i n -> Fib (S i) m -> Fib (S (S i)) (n + m)
  
FibIndex : (i : Nat) -> Type
FibIndex i = Fib i (fib i)

-- λΠ> map fib [0..10]
-- [0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55] : List Nat

fib3 : FibIndex 3
fib3 = FibSucc FibOne (FibSucc FibZero FibOne)

prop : (fb : Fib x y) -> y = fib x
prop FibZero = Refl
prop FibOne = Refl
prop (FibSucc x y) = prf1 (prop x) (prop y)
  where 
    prf2 : (i : Nat) -> plus (fib i) (fib (S i)) = plus (fib (S i)) (fib i)
    prf2 Z = Refl
    prf2 (S k) = rewrite plusCommutative (fib (S k)) (fib (S (S k))) in Refl
    
    prf1 : (n = fib i) -> (m = fib (S i)) -> plus n m = plus (fib (S i)) (fib i)
    prf1 {i} Refl Refl = prf2 i


