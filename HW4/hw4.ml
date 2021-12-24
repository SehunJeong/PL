open Domain
open Eval
open Lang

let runb: exp -> value
= fun pgm -> 
    let v, _ = eval pgm (empty_env, empty_mem) in
    v;;

(* let x = 1 in 
   x + 2 *) 
(* 3 *)
let e1 = LETV ("x", NUM 1, 
           ADD (VAR "x", NUM 2)) in
let v = runb e1 in
print_endline (string_of_value v);;