module Fib2

prop : (n : Nat) -> fib n + fib (S n) = fib (S (S n))
prop Z = Refl
prop (S k) = rewrite plusCommutative (fib (S k)) (plus (fib (S k)) (fib k))
  in Refl
