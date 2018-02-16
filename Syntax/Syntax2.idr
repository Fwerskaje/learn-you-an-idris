module Syntax2 

syntax call [f] on [t] and [a] = f t a 

syntax [test] "?" [t] ":" [e] = if test then t else e

g : Int -> Int -> IO ()
g x y = printLn $ x + y

h : String -> Bool -> IO ()
h x False = printLn x
h x True = printLn ""

checkX : Nat -> String
checkX k = (k > 4) ? ">4" : "<4"

{-

checkX : Nat -> String
checkX k = ifThenElse (k > fromInteger 4) (Delay ">4") (Delay "<4")

-}

listGen : List Nat
listGen = [x * x| x <- [1..5]]

{-

listGen : List Nat
listGen = enumFromTo (fromInteger 1) (fromInteger 5) >>= (\x => pure (x * x))

-}

main : IO ()
main = do
  call g on 10 and 5
  call g on 1 and 3
  call h on "qq" and False
  printLn $ (3 > 4) ? "Yes" : "No"


