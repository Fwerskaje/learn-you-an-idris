module L 

-- Local Variables:
-- idris-load-packages: ("lens")
-- End:

import Lens 
import Control.Category 

record Point where
  constructor MkPoint
  _x : Double
  _y : Double

record Uni where
  constructor MkUnit
  _health : Int
  _position : Point

record Game where
  constructor MkGame
  _score : Int
  _units : List Uni
  _boss : Uni
  
game : Game
game = MkGame 0 [] $ MkUnit 10 $ MkPoint 0 0
  
score : Lens Game Int
score = lens _score (\v, game => record { _score = v } game)

units : Lens Game $ List Uni
units = lens _units (\v, game => record { _units = v } game)

boss : Lens Game Uni
boss = lens _boss (\v, game => record { _boss = v } game)

health : Lens Uni Int
health = lens _health (\v, unit => record { _health = v } unit)

position : Lens Uni Point
position = lens _position (\v, unit => record { _position = v } unit)

x : Lens Point Double
x = lens _x (\v, point => record { _x = v } point)

y : Lens Point Double
y = lens _y (\v, point => record { _y = v } point)

{-

λΠ> (score ^= 3) game
MkGame 3 [] (MkUnit 10 (MkPoint 0 0)) : Game

-}

initialState : Game
initialState = MkGame 0 [
   MkUnit 10 (MkPoint 3.5 7.0),
   MkUnit 15 (MkPoint 1.0 1.0),
   MkUnit 8  (MkPoint 0.0 2.1)
  ] 
  (MkUnit 100 
    (MkPoint 0.0 0.0))
    

