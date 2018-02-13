module Stuff

data HList : List Type -> Type where
  Nil : HList []
  (::) : t -> HList ts -> HList $ t :: ts
  
hlist : HList [Nat, String, Char, Type]
hlist = [0, "0", 'O', Bool]

singleton : (t : Type) -> (x : t) -> HList [t]
singleton t x = [x]

