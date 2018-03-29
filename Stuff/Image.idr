module Image

data Image : (f : a -> b) -> b -> Type where
  Im : {f : a -> b} -> (x : a) -> Image f (f x)
 
inv : (f : a -> b) -> (y : b) -> Image f y -> a
inv f (f x) (Im x) = x

r1 : Bool
r1 = Force $ inv ((||) False) True (Im True)

