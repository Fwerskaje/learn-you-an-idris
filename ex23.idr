module ex23 

%default total

data InfIO : Type where
  Do : IO a -> (a -> Inf InfIO) -> InfIO
  
(>>=) : IO a -> (a -> Inf InfIO) -> InfIO 
(>>=) = Do

data Fuel = Dry | More Fuel

partial forever : Fuel
forever = More forever

tank : Nat -> Fuel
tank Z = Dry
tank (S k) = More $ tank k

run : Fuel -> InfIO -> IO ()
run Dry (Do x f) = putStrLn "Out of fuel"
run (More fuel) (Do y f) = y >>= \res => run fuel $ f res

loopPrint : String -> InfIO
loopPrint msg = do putStrLn msg
                   loopPrint msg

{-

λΠ> :printdef loopPrint
loopPrint : String -> InfIO
loopPrint msg = putStrLn msg >>= (\__bindx => Delay (loopPrint msg))

-}
