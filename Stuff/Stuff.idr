module Stuff

data HList : List Type -> Type where
  Nil : HList []
  (::) : t -> HList ts -> HList $ t :: ts
  
hlist : HList [Nat, String, Char, Type]
hlist = [0, "0", 'O', Bool]

singleton : (t : Type) -> (x : t) -> HList [t]
singleton t x = [x]

id' : {a : Type} -> a -> a
id' x = x

answer : (ts : List Type ** HList ts)
answer = ([String, Nat] ** ["Answer: ", 42])

test : Type -> Bool
test Nat = ?test_rhs
test _ = ?t2
