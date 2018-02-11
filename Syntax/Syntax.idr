module Syntax

ifthenelse : (x : Bool) -> Lazy a -> Lazy a -> a;
ifthenelse True  t e = t
ifthenelse False t e = e

syntax IF [test] THEN [t] ELSE [e] = ifthenelse test t e

f : Nat
f = IF True THEN 42 ELSE 1337
