module Ex2 

import System

%default total

readNumber : IO $ Maybe Nat
readNumber = do
  input <- getLine
  if all isDigit $ unpack input 
  then pure $ Just $ cast input
  else pure Nothing

countdown : (secs : Nat) -> IO ()
countdown Z = putStrLn "Lift off!"
countdown s@(S k) = do putStrLn (show s)
                       usleep 1000000
                       countdown k

%default partial

countdowns : IO ()
countdowns = do putStr "Enter starting number: "
                Just startNum <- readNumber | Nothing => do putStrLn "Invalid input"
                                                            countdowns
                countdown startNum
                putStr "Another (y/n)? "
                yn <- getLine
                if yn == "y" then countdowns 
                else pure ()

-- total
guess : (target : Nat) -> IO ()
guess target = do putStr "Number?: "
                  Just n <- readNumber | Nothing => do putStrLn "Invalid input"
                                                       guess target
                  if n == target then putStrLn "WIN!"
                  else do putStrLn "WRONG"
                          guess target
                                                       
                  

