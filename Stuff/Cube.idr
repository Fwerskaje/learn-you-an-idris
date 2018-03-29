module Cube

import Data.Vect

%default total

data Colour = Green 
            | Red 
            | Blue 
            | Orange 
            | White 
            | Yellow

Show Colour where
  show Green = "G"
  show Red = "R"
  show Blue = "B"
  show Orange = "O"
  show White = "W"
  show Yellow = "Y"

coloursTable : Vect 6 (Nat, Colour)
coloursTable =
  [(0, Green),
   (1, Red),
   (2, Blue),
   (3, Orange),
   (4, White),
   (5, Yellow)] 

lookupColour : (n : Nat) -> (Nat, Colour)
lookupColour n = Vect.index k coloursTable 
  where k : Fin 6
        k = let nMod = modNatNZ n (the Nat 7) notZero in
         case natToFin nMod 6 of
           Nothing => FZ -- I dont know, maybe I shoud prove that nMod 6 is always >= 6?
           (Just x) => x
         where notZero : (the Nat 7 = 0) -> Void
               notZero Refl impossible
                 
Enum Colour where
    fromNat = snd . lookupColour 
    toNat Green = 0
    toNat Red = 1
    toNat Blue = 2
    toNat Orange = 3
    toNat White = 4
    toNat Yellow = 5
    pred = fromNat . Nat.pred . toNat

{-
data Scheme = 
  L | R | F | U | D

data Sides = One
           | Two 
           | Three 
        
SidesToNat : Sides -> Nat           
SidesToNat One = 1
SidesToNat Two = 2
SidesToNat Three = 3

colourN : Nat -> Colour -> Colour
colourN n = fromNat . (+n) . toNat

colours : (c : Colour) -> (scm : Scheme) -> (s : Sides) -> Vect (SidesToNat s) Colour
-}

{-
record Cell (coloursNum : Nat) where
  constructor MkCell'
  mainColour : Colour
  colours : Vect coloursNum Colour
  
MkCell : (c : Colour) -> (n : Nat) -> Cell n
MkCell c n = MkCell' c ?d
-}
 
