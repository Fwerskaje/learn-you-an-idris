module Field

import Data.Vect

data Cell = Bactery | Food | None

record FieldArea (columns : Nat) (rows : Nat) where
  constructor MkFieldArea 
  field : Vect columns $ Vect rows Cell

record FieldInformation where
  constructor MkFieldInformation
  population, temperature, food : Nat
  
record Field where
  constructor MkField
  columns : Nat
  rows : Nat
  fieldArea : FieldArea columns rows
  fieldInformation : FieldInformation

fieldToString : Vect columns (Vect rows String) -> String
fieldToString xs = foldr (\vec, acc => (foldr (++) "\n" vec) ++ acc) "" xs

Show Cell where
  show Bactery = "☺"
  show Food = "*"
  show None = " "
  
Show Field where
  show (MkField _ _ (MkFieldArea field)
       (MkFieldInformation population temperature food)) = 
       "POPULATION: " ++ show population ++ 
       " TEMPERATURE: " ++ show temperature ++ 
       " NUMBER OF FOOD: " ++ show food ++ "\n" ++
       fieldToString (map (map show) field)


f : Field
f = MkField 2 2 (MkFieldArea [[Bactery, None],
                              [None, Food]]) (MkFieldInformation 0 0 0)


