module EffTest

import Effects
import Effect.StdIO
import Effect.State

bye : Eff () [STDIO]
bye = putStrLn "Bye!"

helloName : Eff () [STATE Int, STDIO]
helloName = do
  putStr "Name? [or q for quit]: "
  getName <- getStr
  if getName == "q" then bye 
  else do putStrLn $ "Helo, " ++ trim getName ++ "!"
          update (+1)
          putStrLn $ "Number of of greetings: " ++ show !get
          helloName

main : IO ()
main = run helloName

