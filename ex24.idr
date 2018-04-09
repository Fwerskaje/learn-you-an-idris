module ex24

import Control.Monad.State

increase : Nat -> State Nat ()
increase inc = do current <- get
                  put $ current + inc

{-
λΠ> runState (increase 5) 89
((), 94) : ((), Nat)
λΠ> evalState (increase 5) 89
() : ()
λΠ> execState (increase 5) 89
94 : Nat
-}

data Tree a = Empty 
            | Node (Tree a) a (Tree a)
            
testTree : Tree String
testTree = Node (
                Node 
                  (Node Empty "Jim" Empty)
                  "Fred"
                  (Node Empty "Sheila" Empty)
                 )
                  "Alice"
                (
                Node
                  Empty
                  "Bob"
                  (Node Empty "Eve" Empty)
                 )

flatten : Tree a -> List a
flatten Empty = []
flatten (Node left val right) = flatten left ++ val :: flatten right

treeLabelWith : Tree a -> State (Stream labelType) (Tree (labelType, a))
treeLabelWith Empty = pure Empty
treeLabelWith (Node left val right) 
  = do left_labelled <- treeLabelWith left
       (this :: rest) <- get
       put rest
       right_labelled <- treeLabelWith right
       pure (Node left_labelled (this, val) right_labelled)

treeLabel : Tree a -> Tree (Integer, a)
treeLabel tree = evalState (treeLabelWith tree) [1..]
