module Ex3 

import Data.Vect

readVectLen : (len : Nat) -> IO $ Vect len String
readVectLen Z = pure []
readVectLen (S k) = do x <- getLine
                       xs <- readVectLen k
                       pure $ x :: xs
                       
data VectUnknown : Type -> Type where
  MkVect : (len : Nat) -> Vect len a -> VectUnknown a
 
{-

λΠ> MkVect 3 [1,2,3]
MkVect 3 [1, 2, 3] : VectUnknown Integer

-}

{-
readVect : IO $ VectUnknown String
readVect = do x <- getLine
              if x == "" then pure $ MkVect _ []
              else do MkVect _ xs <- readVect 
                      pure $ MkVect _ $ x :: xs
-}

{-
printVect : Show a => VectUnknown a -> IO ()
printVect (MkVect len xs) = putStrLn $ show xs ++ " length " ++ show len
-}

{-

A dependent pair is a more expressive form of this construct, 
where the type of the second element in a pair can be computed from the value of the first element. 

-}

anyVect : (n : Nat ** Vect n Nat)
anyVect = (3 ** [1,2,3])

readVect : IO (len ** Vect len String)
readVect = do x <- getLine
              if x == "" then pure (_ ** [])
              else do (_ ** xs) <- readVect
                      pure (_ ** x :: xs)

zipInputs : IO ()
zipInputs = do putStrLn "First vector: "
               (len1 ** vec1) <- readVect
               putStrLn "Second vector: "
               (len2 ** vec2) <- readVect
               case exactLength len1 vec2 of
                    Nothing => putStrLn "Vectors are different lengths"
                    Just vec2' => printLn $ zip vec1 vec2'
                    
             
                              
