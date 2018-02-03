module Ex1 

%default total

main : IO ()
main = do
  putStr "Enter your name: "
  x <- getLine
  putStrLn $ "Hello " ++ x ++ "!"
  
{-

λΠ> putStrLn $ show $ 47 * 2
io_bind (prim_write "94\n") (\__bindx => io_pure ()) : IO ()

-}  

printLength : IO ()
printLength = getLine >>= \input => putStrLn $ show $ length input

printLength' : IO ()
printLength' = do 
  x <- getLine
  putStrLn $ show $ length x 

{-

Using do notation, write a printLonger program that reads two strings 
and then displays the length of the longer string.

-}

printLonger : IO ()
printLonger = do
  putStr "First string: "
  str1 <- getLine
  putStr "Second string: "
  str2 <- getLine
  putStrLn $ show $ max (length str1) (length str2)

printLonger' : IO ()
printLonger' = putStr "First string: " >>= 
               \_ => getLine >>= \str1 =>
               putStr "Second string: " >>= 
               \_ => getLine >>= \str2 =>
               putStrLn $ show $ maximum (length str1) (length str2)
{-

pureAnswer : IO Nat
pureAnswer = pure 42

Prelude> pure "Hello"
"Hello"
Prelude> :t pure "Hello"
pure "Hello" :: Applicative f => f [Char]
Prelude> pure "Hello" :: IO String
"Hello"

Hmm~

-}

readPair : IO (String, String)
readPair = do str1 <- getLine
              str2 <- getLine
              pure (str1, str2)
           
usePair : IO ()
usePair = do (str1, str2) <- readPair
             putStrLn $ "You entered: " ++ str1 ++ " and " ++ str2
 
readNumber : IO $ Maybe Nat
readNumber = do
  input <- getLine
  if all isDigit $ unpack input 
  then pure $ Just $ cast input
  else pure Nothing
                                     
readNumbers : IO $ Maybe (Nat, Nat)
readNumbers = do Just num1 <- readNumber | Nothing => pure Nothing
                 Just num2 <- readNumber | Nothing => pure Nothing
                 pure $ Just (num1, num2)
                 --?result

{-                 
                      
λΠ> :exec readNumbers >>= printLn                  
bad

Process idris~ segmentation fault: 11
:D
                 
-}                



