open Domain
open Eval
open Lang

let run: program -> value
= fun pgm -> 
    eval pgm empty_env;;

(* letrec
 *   even (x) = if zero? (x) then 1 else (odd (x-1))
 *   odd (x) = if zero? (x) then 0 else (even (x-1))
 * in (odd 13) (* true *)
 * *)
let e12 = LETMREC(
            "even", "x", IF(ISZERO(VAR "x"), CONST 1, APPLY(VAR "odd", SUB(VAR "x", CONST 1))),
            "odd", "x", IF(ISZERO(VAR "x"), CONST 0, APPLY(VAR "even", SUB(VAR "x", CONST 1))),
            APPLY(VAR "odd", CONST 13)) in
print_endline (string_of_value (run e12));;

(* letrec double(x) = 
 *   if iszero(x) then 0 else ((double (x-1)) + 1)
 * in (double 1) *) (* 2 *)
let e11 = LETREC("double", "x",
            IF(ISZERO(VAR "x"), CONST 0, ADD(APPLY(VAR "double", SUB(VAR "x", CONST 1)), CONST 2)),
            APPLY(VAR "double", CONST 1)) in
print_endline (string_of_value (run e11));;

(* let x = 1
 * in let f = proc (y) (x+y)
 *    in let x = 2
 *       in (f 3) *) (* 4 *)
let e10 = LET("x", CONST 1, 
            LET ("f", PROC ("y", ADD(VAR "x", VAR "y")), 
              LET ("x", CONST 2, APPLY(VAR "f", CONST 3)))) in
print_endline (string_of_value (run e10));;

(* (proc (x) (x)) 1 *) (* 1 *)
let e9 = APPLY(PROC ("x", VAR "x"), CONST 1) in
print_endline (string_of_value (run e9));;

(* let x = 1 in x + 2 *) (* 3 *)
let e1 = LET ("x", CONST 1, ADD (VAR "x", CONST 2)) in
print_endline (string_of_value (run e1));;

(* let x = 1 in let y = 2 in x + y *) (* 3 *)
let e2 = LET ("x", CONST 1, LET ("y", CONST 2, ADD (VAR "x", VAR "y"))) in
print_endline (string_of_value (run e2));;

(* let x = let y = 2 
 *         in y + 1 
 * in x + 3 *) (* 6 *)
 let e5 = LET ("x", LET ("y", CONST 2, ADD (VAR "y", CONST 1)), ADD (VAR "x", CONST 3)) in
 print_endline (string_of_value (run e5));;

(* let x = 1 
 * in let y = 2 
 * in let x = 3 
 * in x + y *) (* 5 *)
 let e6 = LET ("x", CONST 1, LET ("y", CONST 2, LET ("x", CONST 3, ADD (VAR "x", VAR "y")))) in
 print_endline (string_of_value (run e6));;

(* let x = 1 
 * in let y = let x = 2 
 *            in x + x 
 * in x + y *) (* 5 *)
 let e7 = LET ("x", CONST 1, LET ("y", LET ("x", CONST 2, ADD (VAR "x", VAR "x")), ADD (VAR "x", VAR "y"))) in
 print_endline (string_of_value (run e7));;

(* let x = 1 
 * in let y = 2 
 * in if iszero (x - 1) then 
 *   y - 1 
 * else 
 *   y + 1 *) (* 1 *)
 let e8 = LET ("x", CONST 1, LET ("y", CONST 2, IF (ISZERO (SUB (VAR "x", CONST 1)), SUB (VAR "y", CONST 1), ADD (VAR "y", CONST 1)))) in
 print_endline (string_of_value (run e8));;

(* let x = 7 
 * in let y = 2 
 * in let y = let x = x - 1 
 *            in x - y 
 * in (x-8)-y *) (* -5 *)
let e3 = LET ("x", CONST 7, LET ("y", CONST 2, LET ("y", LET ("x", SUB (VAR "x", CONST 1), SUB (VAR "x", VAR "y")), SUB (SUB (VAR "x", CONST 8), VAR "y")))) in
print_endline (string_of_value (run e3));;

(* let x = 1 in let y = iszero x in x + y *) (* exception *)
let e4 = LET ("x", CONST 1, LET ("y", ISZERO (VAR "x"), ADD (VAR "x", VAR "y"))) in
print_endline (string_of_value (run e4));;


