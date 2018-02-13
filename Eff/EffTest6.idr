module Main--EffTest6

import Effects
import Effect.File
import Effect.StdIO
import Effect.System

readFile : Eff (List String) [FILE R]
readFile = readAcc [] where
  readAcc : List String -> Eff (List String) [FILE R]
  readAcc acc = if not !eof then do
                   Result s <- readLine
                   readAcc $ s :: acc
                else pure $ reverse acc

dumpFile : String -> Eff () [FILE (), STDIO]
dumpFile name = do 
  Success <- open name Read | FError e => putStrLn "Error!"
  ss <- readFile
  mapE (\s => putStr s) ss
  close
  
emain : Eff () [FILE (), SYSTEM, STDIO] 
emain = do
  [prog, n] <- getArgs | [] => putStrLn "Can't happend!"
                       | [prog] => putStrLn "No arguments!"
                       | _ => putStrLn "To many arguments"
  dumpFile n

main : IO ()
main = run emain
