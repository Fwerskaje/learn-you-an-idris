module Fu

%hide Functor

-- https://typeclasses.com/contravariance

interface Functor (f : Type -> Type) where
  fmap : (z0 -> z1) -> (f z0 -> f z1)
  
interface Contravariant (f : Type -> Type) where
  contramap : (a0 -> a1) -> (a1 -> a0)

contraComp : (a -> b) -> (b -> c) -> a -> c
contraComp f g x = g (f x)
      
-- newtype Predicate a = Predicate {getPredicate :: a -> Bool}

record Predicate (a : Type) where
  constructor MkPredicate
  getPredicate : a -> Bool
  
Contravariant Predicate where
  contramap f = ?h_2
  
-- newtype Op z a = Op {getOp :: a -> z}

record Op (z : Type) (a : Type) where
  constructor MkOp
  getOp : a -> z
  
-- newtype Equivalence a = Equivalence {getEquivalence :: a -> a -> Bool}
  
record Equivalence (a : Type) where
  constructor MkEquivalence 
  getEquivalence : a -> a -> Bool
  
-- newtype Comparison a = Comparison {getComparison :: a -> a -> Ordering}

record Comparison (a : Type) where
  constructor MkComparasion
  getComparison : a -> a -> Ordering
  

