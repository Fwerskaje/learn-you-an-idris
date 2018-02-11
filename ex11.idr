module Ex11 

data Expr num = Val num
              | Add (Expr num) (Expr num)
              | Sub (Expr num) (Expr num)
              | Mul (Expr num) (Expr num)
              | Div (Expr num) (Expr num)
              | Abs (Expr num)

Num ty => Num (Expr ty) where
  (+) = Add
  (*) = Mul
  fromInteger = Val . fromInteger
  
Neg ty => Neg (Expr ty) where
  negate x = 0 - x
  (-) = Sub

Abs ty => Abs (Expr ty) where
  abs = Abs
  
Functor Expr where 
  map func (Val x) = Val $ func x
  map func (Add x y) = Add (map func x) (map func y)
  map func (Sub x y) = Sub (map func x) (map func y)
  map func (Mul x y) = Mul (map func x) (map func y)
  map func (Div x y) = Div (map func x) (map func y)
  map func (Abs x) = Abs $ map func x

eval : (Neg num, Integral num, Abs num) => Expr num -> num 
eval (Val x) = x
eval (Add x y) = (+) (eval x) (eval y)
eval (Sub x y) = (-) (eval x) (eval y)
eval (Mul x y) = (*) (eval x) (eval y)
eval (Div x y) = div (eval x) (eval y)
eval (Abs x) = abs $ eval x

expression : Expr Int
expression = Add (Val 6) (Mul (Val 3) (Val 12))

{-

λΠ> eval expression
42 : Int

-}

{-

λΠ> the (Expr _) (6 + 3 * 12)
Add (Val 6) (Mul (Val 3) (Val 12)) : Expr Integer

λΠ> eval (6 + 3 * 12)
42 : Integer

-}

Cast (Maybe elem) (List elem) where
  cast Nothing = []
  cast (Just x) = [x]
  
ex1 : List Integer
ex1 = cast $ Just 3

{-

λΠ> ex1
[3] : List Integer

-}

showOperator : Show ty => String -> ty -> ty -> String
showOperator op x y = "(" ++ show x ++ " " ++ op ++ " " ++ show y ++ ")"


Show ty => Show (Expr ty) where
  show (Val x) = show x
  show (Add x y) = showOperator "+" x y
  show (Sub x y) = showOperator "-" x y
  show (Mul x y) = showOperator "*" x y
  show (Div x y) = showOperator "div" x y
  show (Abs x) = "(abs " ++ show x ++ ")"
  
{-

λΠ> show (the (Expr _) (6 + 3 * 12))
"(6 + (3 * 12))" : String
λΠ> show (the (Expr _) (6 * 3 + 12))
"((6 * 3) + 12)" : String

-}

(Neg ty, Integral ty, Eq ty, Abs ty) => Eq (Expr ty) where
    (==) x y = eval x == eval y

{-

λΠ> the (Expr _) (2 + 4) == 3 + 3
True : Bool
λΠ> the (Expr _) (2 + 4) == 3 + 4
False : Bool

-}

data Tree elem = Empty 
               | Node (Tree elem) elem (Tree elem)
               
Functor Tree where
  map func Empty = Empty
  map func (Node x y z) = Node (map func x) (func y) (map func z)
  
totalLen : List String -> Nat
totalLen xs = foldr (\str, acc => length str + acc) 0 xs

Foldable Tree where
  foldr func acc Empty = acc
  foldr func acc (Node x y z) = let left = foldr func acc x
                                    right = foldr func left z in
                                    func y right
                                    
treeSum : Integer
treeSum = foldr (+) 0 tree 
  where
  tree : Tree Integer
  tree = Node (Node Empty 3 (Node Empty 4 Empty)) 0 Empty

