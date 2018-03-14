module Rn

isZero : Dec (Z = Z)
isZero = Yes Refl

notZero : Dec (S k = 0)
notZero = No notZ
  where notZ : (S k = 0) -> Void
        notZ Refl impossible

isZeroProof : (x : Nat) -> Dec (x = Z)
isZeroProof Z = isZero
isZeroProof (S k) = notZero

main : IO ()
main = let convert = map ((the Nat) . cast) in do
  num1 <- convert getLine
  num2 <- convert getLine
  case isZeroProof num2 of
    (Yes prf) => putStrLn "It's zero!"
    (No contra) => let res = modNatNZ num1 num2 contra in
      putStrLn $ "Result: " ++ show res
         
        
    
  
