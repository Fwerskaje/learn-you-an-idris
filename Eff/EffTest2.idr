module Main--EffTest2 

import Data.Vect

import Effects
import Effect.StdIO
import Effect.Random

dice3 : Eff (Vect 3 Integer) [RND]
dice3 = do
  let num = rndInt 1 6
  pure [!num, !num, !num]
 
cast_dice : Nat -> Eff () [RND, STDIO]
cast_dice Z = pure ()
cast_dice (S n) = do
  xs <- dice3
  printLn xs
  cast_dice n

main : IO ()
main = run $ cast_dice 5
