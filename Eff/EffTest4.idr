module Main--EffTest4

import Effects
import Effect.State
import Effect.Exception
import Effect.Random
import Effect.StdIO

data Expr = Val Integer
          | Var String
          | Add Expr Expr
          | Random Integer

Env : Type
Env = List (String, Integer)

EvalEff : Type -> Type
EvalEff t = Eff t [EXCEPTION String, STATE Env, RND, STDIO]

eval : Expr -> EvalEff Integer
eval (Val n) = pure n
eval (Add i j) = pure $ !(eval i) + !(eval j)
eval (Random n) = do v <- rndInt 0 n; putStrLn ("Random: " ++ show v); pure v
eval (Var n) = case lookup n !get of
                    Nothing => raise $ "No such variable " ++ n
                    (Just x) => pure x
{-                        
eval : Expr -> Integer
eval (Val x) = x
eval (Add x y) = (+) (eval x) (eval y)
-}

runEval : List (String, Integer) -> Expr -> IO Integer
runEval args expr = run $ eval' expr
  where eval' : Expr -> EvalEff Integer
        eval' e = do put args
                     eval e

main : IO ()
main = do 
  putStrLn "Expression 1:"
  r <- runEval [("a", 5)] (Add (Var "a") (Random 10))
  printLn r
  putStrLn "Expression 2:"
  r2 <- runEval [] (Add (Add (Random 42) (Random 1337)) (Random 15))
  printLn r2
  putStrLn "Expression 3:"
  r3 <- runEval [("k", 42), ("c", 54)] (Add (Var "a") (Var "c"))
  printLn r3
  -- printLn "Never printed"
  
