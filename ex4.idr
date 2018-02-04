module Ex4

import Data.Vect

tri : Vect 3 (Double, Double)
tri = [(0.0, 0.0), (3.0, 0.0), (0.0, 4.0)]

Position : Type
Position = (Double, Double)

tri' : Vect 3 Position
tri' = [(0.0, 0.0), (3.0, 0.0), (0.0, 4.0)]

Polygon : Nat -> Type
Polygon n = Vect n Position

tri'' : Polygon 3
tri'' = [(0.0, 0.0), (3.0, 0.0), (0.0, 4.0)]

{-

λΠ> Polygon 3
Vect 3 (Double, Double) : Type

-}

StringOrInt : Bool -> Type
StringOrInt False = String
StringOrInt True = Int

getStringOrInt : (isInt : Bool) -> StringOrInt isInt
getStringOrInt False = "Ninety four"
getStringOrInt True = 94

{-
valToString : (isInt : Bool) -> StringOrInt isInt -> String
valToString False x = trim x
valToString True x = cast x
-}

valToString : (isInt : Bool) -- -> (case isInt of False => String; True => Int)
  -> (if isInt then Int else String)
  -> String
valToString False x = trim x
valToString True x = cast x

AdderType : (numargs : Nat) -> Type -> Type
AdderType Z numType = numType
AdderType (S k) numType = (next : numType) -> AdderType k numType

adder : Num numType => (numargs : Nat) -> numType -> AdderType numargs numType
adder Z acc = acc
adder (S k) acc = \next => adder k $ next + acc










