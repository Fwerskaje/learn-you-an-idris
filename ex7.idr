module Ex7

import Data.Vect

infixr 5 .+.

data Schema = SString | SInt | (.+.) Schema Schema

SchemaType : Schema -> Type
SchemaType SString = String
SchemaType SInt = Int
SchemaType (x .+. y) = (SchemaType x, SchemaType y)

{-
data DataStore : Type where
  MkData : (schema : Schema) -> 
  (size : Nat) -> 
  (items : Vect size (SchemaType schema)) -> 
  DataStore
  

λΠ> SchemaType (SInt .+. SString)
(Int, String) : Type


size : DataStore -> Nat
size (MkData schema size items) = size

schema : DataStore -> Schema
schema (MkData schema size items) = schema

items : (store : DataStore) -> Vect (size store) $ SchemaType $ schema store
items (MkData schema size items) = items
-}

record DataStore where
  constructor MkData
  schema : Schema
  size : Nat
  items : Vect size $ SchemaType schema

{-

λΠ> :let teststore = (MkData (SString .+. SInt) 1 [("Answer", 42)])
defined
λΠ> :t teststore
teststore : DataStore

λΠ> schema teststore
SString .+. SInt : Schema
λΠ> size teststore
1 : Nat
λΠ> items teststore
[("Answer", 42)] : Vect 1 (String, Int)

-}

data Command : Schema -> Type where
  Add : SchemaType schema -> Command schema
  Get : Integer -> Command schema
  Quit : Command schema

parsePrefix : (schema : Schema) -> String -> Maybe (SchemaType schema, String)

parseBySchema : (schema : Schema) -> String -> SchemaType schema
parseBySchema schema input = case parsePrefix schema input of
                                  Just (res, "") => Just res
                                  Just _ => Nothing
                                  Nothing => Nothing

parseCommand : (schema : Schema) -> String -> String -> Maybe $ Command schema
parseCommand schema "add" rest = case parseBySchema schema rest of
                                      Nothing => Nothing
                                      Just restok => Just $ Add restok
parseCommand schema "get" val = if all isDigit $ unpack val then Just $ Get $ cast val else Nothing 
parseCommand schema "quit" "" = Just Quit
parseCommand _ _ _ = Nothing

parse : (schema : Schema) -> (input : String) -> Maybe $ Command schema
parse schema input = case span (/= ' ') input of 
                          (cmd, args) => parseCommand schema cmd $ ltrim args

addToStore : (store : DataStore) -> SchemaType (schema store) -> DataStore
addToStore (MkData schema size store) newitem = 
  MkData schema (size + 1) $ store ++ [newitem] 
  -- if newitem :: store ⇒ (S size)
  -- if store ++ [newitem] ⇒ (size + 1)
  
  --addToData store
  {-
    where
    addToData : Vect oldsize (SchemaType schema) ->
                Vect (S oldsize) (SchemaType schema)
                
    addToData [] = [newitem]
    addToData (item :: items) = item :: addToData items 
                                                       -}

display : SchemaType schema -> String
display {schema = SString} item = show item
display {schema = SInt} item = show item
display {schema = (x .+. y)} (iteml, itemr) = display iteml ++ ", " ++ display itemr

getEntry : (pos : Integer) -> (store : DataStore) -> Maybe (String, DataStore)
getEntry pos store = 
  case integerToFin pos (size store) of
      Nothing => Just ("Out of range\n", store)
      Just idItem => Just (display (index idItem (items store)) ++ "\n", store)    
                                                                        
processInput : DataStore -> String -> Maybe (String, DataStore)
processInput store input = case parse (schema store) input of 
  Nothing => Just ("Invalid command\n", store)
  Just (Add item) => Just ("ID " ++ show (size store) ++ "\n", addToStore store (?convert item))
  Just (Get pos) => getEntry pos store 
  Just Quit => Nothing

main : IO ()
main = replWith (MkData SString _ []) "Command: " processInput

