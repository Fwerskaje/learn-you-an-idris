module L 

-- Local Variables:
-- idris-load-packages: ("lens")
-- End:

import Lens 

record Point where
  constructor MkPoint
  _x : Int
  _y : Int 

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

{-

λΠ> (score ^= 3) game
MkGame 3 [] (MkUnit 10 (MkPoint 0 0)) : Game

-}
