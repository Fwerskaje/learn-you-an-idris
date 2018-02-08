module Ex8

maybeAdd : Maybe Int -> Maybe Int -> Maybe Int
maybeAdd x y = x >>= \x_val =>
               y >>= \y_val =>
               Just $ x_val + y_val

maybeAdd' : Maybe Int -> Maybe Int -> Maybe Int
maybeAdd' x y = do x_val <- x
                   y_val <- y
                   Just $ x_val + y_val
                   
double : Num a => a -> a
double x = x + x

occurrences : Eq ty => (item : ty) -> (values : List ty) -> Nat
occurrences item [] = 0
occurrences item (value :: values) = case value == item of
                                          False => occurrences item values
                                          True => 1 + occurrences item values
                                          
data Matter = Solid | Liquid | Gas

Eq Matter where
  (==) Solid Solid = True
  (==) Liquid Liquid = True
  (==) Gas Gas = True
  (==) _ _ = False
  --(/=) x y = not $ x == y

data Tree elem = Empty 
               | Node (Tree elem) elem (Tree elem)

Eq elem => Eq (Tree elem) where
  (==) Empty Empty = True
  (==) (Node x z w) (Node y s t) = x == y && z == s && w == t
  (==) _ _ = False

