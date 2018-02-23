module Ex9

data Command = Add String 
             | Get Integer
             | Quit
             
parseCommand : (cmd : String) -> (args : String) -> Maybe Command

parse : (input : String) -> Maybe Command
parse input = case span (/= ' ') input of
                   (cmd, args) => parseCommand cmd args
