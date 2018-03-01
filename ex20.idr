module Ex20

import Data.Primitives.Views

labelWith : Stream labelType -> List a -> List (labelType, a)
labelWith xs [] = []
labelWith (value :: xs) (x :: ys) = (value, x) :: labelWith xs ys

label : List a -> List (Integer, a)
label = labelWith $ iterate (+1) 0

quiz : Stream Int -> (score : Nat) -> IO ()
quiz (num1 :: num2 :: nums) score = do 
  putStrLn $ "Score so far: " ++ show score
  putStr $ show num1 ++ " * " ++ show num2 ++ "? "
  answer <- getLine
  if cast answer == num1 * num2 then do
    putStrLn "Correct!"
    quiz nums $ succ score
  else do
    putStrLn $ "Wrong, the answer is " ++ show (num1 * num2)
    quiz nums score
    
runQuiz : IO ()
runQuiz = quiz (iterate (+1) 0) 0  
              
randoms : Int -> Stream Int
randoms seed = (seed' `shiftR` 2) :: randoms seed'
  where seed' = 1664525 * seed + 1013904223

arithInputs : Int -> Stream Int
arithInputs seed = map bound $ randoms seed
  where bound : Int -> Int
        bound num with (divides num 12)
          bound ((12 * div) + rem) | (DivBy _) = rem + 1

runRandomQuiz : IO ()
runRandomQuiz = quiz (arithInputs 12345) 0 

--------

every_other : Stream a -> Stream a
every_other (_ :: x :: xs) = x :: every_other xs

{-

λΠ> take 10 (every_other [1..])
[2, 4, 6, 8, 10, 12, 14, 16, 18, 20] : List Integer

-}

data Face = Heads | Tails

Show Face where
  show Heads = "Heads"
  show Tails = "Tails"

coinFlips : (count : Nat) -> Stream Int -> List Face
coinFlips count xs = take count $ map getFace xs
  where getFace : Int -> Face
        getFace x = if mod x 2 > 0 then Heads else Tails

printCoins : IO ()
printCoins = traverse_ printLn $ coinFlips 6 $ randoms 12345

{-

Heads
Tails
Heads
Tails
Tails
Heads

-}


