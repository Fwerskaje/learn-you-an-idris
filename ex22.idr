module Main

%include C "limits.h"

-- access the preprocessor definition INT_MAX
intMax : IO Int
intMax = foreign FFI_C "#INT_MAX" (IO Int)

main : IO ()
main = print !intMax
