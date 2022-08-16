open Lang
open Type
open Domain

let t1 = PROC("x", ADD(VAR "x", CONST 1));;
print_endline (string_of_type(typeof(CONST 1)));; 

