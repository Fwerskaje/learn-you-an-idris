module Univ

data HList : List Type -> Type where
  Nil : HList []
  (::) : t -> HList ts -> HList $ t :: ts

-- the Universe pattern
data Column = TEXT | REAL | INTEGER

interpCol : Column -> Type
interpCol TEXT = String
interpCol REAL = Double
interpCol INTEGER = Integer

Table : Type
Table = List (String, Column)

interp : Table -> Type
interp tbl = HList (map (interpCol . snd) tbl)

PeopleSchema : Table
PeopleSchema = [("Name", TEXT), ("Hamster", INTEGER)]

People : List $ interp PeopleSchema
People = [["David", 1], ["Edwin", 0]]

countHamsters : List (interp PeopleSchema) -> Integer
countHamsters [] = 0
countHamsters ([_, h] :: xs) = h + countHamsters xs


