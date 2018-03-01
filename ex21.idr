module Main

import Effects
import Effect.System
import Effect.StdIO

%include C "curses.h" -- FUCK
%lib C "curses"

initScr : Eff () [STDIO]
initScr = foreign FFI_C "initscr" (Eff () [STDIO])

printw : String -> Eff () [STDIO]
printw str = foreign FFI_C "printw" (String -> IO ()) str

refresh : Eff () [STDIO]
refresh = foreign FFI_C "refresh" (IO ())

clear : Eff () [STDIO]
clear = foreign FFI_C "clear" (IO ())

endwin : Eff () [STDIO]
endwin = foreign FFI_C "endwin" (IO ())


oneScreenPrint : (slide : String) -> (delay : Nat) -> Eff () [STDIO, SYSTEM]
oneScreenPrint slide delay = 
  do printw slide
     refresh
     usleep (cast delay)
     clear 

{-
animate : (slides : List String) -> (delay : Int) -> IO ()
animate slides delay = do initScr; animate_helper xs x
  where animate_helper : List String -> Int -> IO ()
        animate_helper [] = endwin
        animate_helper (slide :: nextslide) = do oneScreenPrint x
                                animate_helper xs
-}

main : IO ()
main = ?h

          

-- Local Variables:
-- idris-load-packages: ("effects")
-- End:
