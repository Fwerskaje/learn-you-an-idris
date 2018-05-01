module Ex1

%default total

rev : a = b -> b = a
rev Refl = Refl

plusZero : (a : Nat) -> a + 0 = a
plusZero Z = Refl
plusZero (S k) = rewrite plusZero k in Refl 

sPlus : (k : Nat) -> (b : Nat) -> S (k + b) = k + (S b)
sPlus Z b = Refl
sPlus (S k) b = rewrite sPlus k b in Refl

sKeqPlusKOne : (k : Nat) -> S k = k + 1
sKeqPlusKOne Z = Refl
sKeqPlusKOne (S k) = rewrite sKeqPlusKOne k in Refl

aPlusSb : (a : Nat) -> (b : Nat) -> a + (S b) = (S a) + b
aPlusSb Z b = Refl
aPlusSb (S k) b = rewrite aPlusSb k b in Refl

-- Commutativity
partial -- wtf?
natComm : (a : Nat) -> (b : Nat) -> a + b = b + a
natComm Z b = rewrite plusZero b in Refl
natComm (S k) j = rewrite sPlus k j in 
                  rewrite natComm j (S k) in 
                  rewrite aPlusSb k j in Refl

natAss : (a : Nat) -> (b : Nat) -> (c : Nat) -> 
     (a + b) + c = a + (b + c)
natAss Z b c = Refl
natAss (S k) b c = rewrite natAss k b c in Refl

-- plusZero

-- for Integers:

postulate 
intComm : (a : Int) -> (b : Int) -> a + b = b + a

postulate 
intAssoc : (a : Int) -> (b : Int) -> (c : Int) -> (a + b) + c = a + (b + c)

postulate 
plusZeroInt : (a : Int) -> a + 0 = a

postulate 
plusAminusA : (a : Int) -> a + (- a) = 0

--                                                                         c = b - a
subInt : (a : Int) -> (b : Int) -> (c : Int) -> c + a = b -> (c + a) + (- a) = b + (- a)
subInt a b c prf = 
  rewrite intAssoc c a (- a) in 
  rewrite plusAminusA a in 
  let prf2 = rev prf in 
  rewrite prf2 in 
  rewrite intAssoc c a (0 - a) in 
  rewrite plusAminusA a in Refl

postulate 
intMulComm : (a : Int) -> (b : Int) -> a * b = b * a

postulate 
intMulAssoc : (a : Int) -> (b : Int) -> (a * b) * c = a * (b * c)

postulate 
intMulOne : (a : Int) -> a * 1 = a

postulate 
powMinusOne : (a : Double) -> Not (a = 0.0) -> a * (pow a (-1.0)) = 1.0

interface AdditiveAbelianGroup ty where
  (+) : ty -> ty -> ty
  neg : ty -> ty
  zero : ty
  comm : (a : ty) -> (b : ty) -> a + b = b + a
  assoc : (a : ty) -> (b : ty) -> (c : ty) -> (a + b) + c = a + (b + c)
  plusZ : (a : ty) -> a + zero = a
  minus : (a : ty) -> a + (neg a) = a
  
