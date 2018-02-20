module Ex15 

import Data.Vect

data WordState : (guesses_remaining : Nat) -> (letters : Nat) -> Type where
  MkWordState : (word : String) -> 
                (missing : Vect letters Char) -> 
                WordState guesses_remaining letters  

data Finished : Type where
  Lost : (game : WordState 0 (S letters)) -> Finished
  Won  : (game : WordState (S guesses) 0) -> Finished
  
data ValidInput : List Char -> Type where
  Letter : (c : Char) -> ValidInput [c]

isValidInput : (cs : List Char) -> Dec $ ValidInput cs
isValidInput [] = No zeroInput
  where zeroInput : ValidInput [] -> Void
        zeroInput (Letter _) impossible
        
isValidInput (x :: []) = Yes (Letter x)
isValidInput (x :: (y :: xs)) = No notOneCharPrf
  where notOneCharPrf : ValidInput (x :: (y :: xs)) -> Void
        notOneCharPrf (Letter _) impossible

isValidString : (s : String) -> Dec $ ValidInput $ unpack s
isValidString s = isValidInput $ unpack s

readGuess : IO (x ** ValidInput x)
readGuess = do putStr "Guess:"
               x <- getLine
               case isValidString $ toUpper x of
                 (Yes prf) => pure (_ ** prf)
                 (No contra) => do putStrLn "Invalid guess"
                                   readGuess

namespace re -- remove elem
                               
  removeElem : (value : a) -> (xs : Vect (S n) a) -> (prf : Elem value xs) -> Vect n a
  removeElem value (value :: ys) Here = ys
  removeElem {n = Z} value (y :: []) (There later) = absurd later
  removeElem {n = (S k)} value (y :: ys) (There later) = y :: removeElem value ys later

  removeElem_auto : (value : a) -> (xs : Vect (S n) a) -> {auto prf : Elem value xs} -> Vect n a
  removeElem_auto value xs {prf} = removeElem value xs prf

processGuess : (letter : Char) -> 
               WordState (S guesses) (S letters) -> 
               Either (WordState guesses (S letters)) (WordState (S guesses) letters)
processGuess letter (MkWordState word missing) = case isElem letter missing of
                                                      (Yes _) => Right $ MkWordState word $ re.removeElem_auto letter missing
                                                      (No _) => Left $ MkWordState word missing
                                                      
game : WordState (S guesses) (S letters) -> IO Finished
game {guesses} {letters} st = do (_ ** Letter letter) <- readGuess
                                 case processGuess letter st of
                                   (Left l) => do putStrLn "Wrong!"
                                                  case guesses of 
                                                    Z => pure $ Lost l
                                                    (S k) => game l
                                   (Right r) => do putStrLn "Right!"
                                                   case letters of
                                                     Z => pure $ Won r
                                                     (S k) => game r
{-
wordToVec : (wordToGuess : String) -> Maybe $ Vect wordLen Char
wordToVec wordToGuess = let wordChars = unpack wordToGuess in
                            case nonEmpty $ wordChars of
                              (Yes prf) => Just $ ?sameLength $ fromList wordChars
                              (No _) => Nothing
-}
main : IO ()
main = do --printLn "DEBUG"
          --printLn "Word to guess: "
          --wordToGuess <- getLine 
          --if (length wordToGuess == 0) then main else do
          result <- game {guesses = 2} $ MkWordState "Test" ['T', 'E', 'S']
          case result of 
                      Lost $ MkWordState word missing => putStrLn $ "You lose. The word was " ++ word
                      Won game => putStrLn "You win!"


