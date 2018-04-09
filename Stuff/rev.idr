module rev

-- map f (rev l) = rev (map f l).



p2 : (xs : List a) -> (f : a -> b) -> reverse (map f xs) = (reverse . map f) xs
p2 xs f = Refl

p3 : (xs : List a) -> (f : a -> b) -> map f (reverse xs) = (map f . reverse) xs
p3 xs f = Refl

p : (xs : List a) -> (f : a -> b) -> map f (reverse xs) = reverse (map f xs)
p xs f = rewrite p2 xs f in ?h
 
