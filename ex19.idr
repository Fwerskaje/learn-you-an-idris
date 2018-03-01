module Ex19

labelFrom : Integer -> (xs : List a) -> List (Integer, a)
labelFrom x [] = []
labelFrom x (y :: xs) = (x, y) :: labelFrom (succ x) xs

label : List a -> List (Integer, a)
label xs = labelFrom 0 xs

data InfList : Type -> Type where
  (::) : (value : elem) -> Inf (InfList elem) -> InfList elem
  
%name InfList xs, ys, zs

countFrom : Integer -> InfList Integer
countFrom x = x :: Delay (countFrom (x + 1))

getPrefix : (count : Nat) -> InfList a -> List a
getPrefix Z xs = []
getPrefix (S k) (value :: xs) = value :: getPrefix k xs

label' : List a -> List (Integer, a)
label' xs = zip (getPrefix (length xs) (countFrom 0)) xs

Functor InfList where
  map f (value :: xs) = f value :: map f xs

l : (f : Integer -> Integer) -> List Integer
l f = getPrefix 10 $ map f $ countFrom 0
