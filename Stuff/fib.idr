module Fib 

%default total

data Fib : (i : Nat) -> (fb : Nat) -> Type where
  FibZero : Fib 0 0
  FibOne  : Fib 1 1
  FibSucc : Fib i n -> Fib (S i) m -> Fib (S (S i)) (n + m)

prop : (fb : Fib x y) -> y = fib x
prop FibZero = Refl
prop FibOne = Refl
prop (FibSucc x y) = prf (prop x) (prop y)
  where prf : (n = fib i) -> (m = fib (S i)) -> plus n m = plus (fib (S i)) (fib i)
        prf {i} Refl Refl = rewrite plusCommutative (fib i) (fib (S i)) in Refl 


