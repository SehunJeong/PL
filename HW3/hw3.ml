open Lang
open Value_env
open Eval

let runml: program -> value
= fun pgm ->
    eval pgm empty_env;;

(* let x = 1
 * in let f = proc (y) (x + y)
 *   in let x = 2
 *     in let g = proc (y) (x + y)
 *       in (f 1) + (g 1) 
 * Int 5 *)

let ex1 = LET ("x", CONST 1,
            LET ("f", PROC ("y", ADD (VAR "x", VAR "y")),
              LET ("x", CONST 2,
                LET ("g", PROC ("y", ADD (VAR "x", VAR "y")),
                  ADD (CALL (VAR "f", CONST 1), CALL (VAR "g", CONST 1)))))) in
print_endline (string_of_value (runml ex1));;


(* letrec double(x) = if (x = 0) then 0 else (double (x-1) + 2
 * in (double 6)
 * Int 12 *)

let ex2 = LETREC ("double", "x",
            IF (EQUAL (VAR "x", CONST 0), CONST 0,
              ADD (CALL (VAR "double", SUB (VAR "x", CONST 1)), CONST 2)),
            CALL (VAR "double", CONST 6)) in
print_endline (string_of_value (runml ex2));;


(* letrec even(x) = if (x = 0) then true else odd(x-1)
 *        odd(x) = if (x = 0) then false else even(x-1)
 * in (even 13)
 * Bool true *)

let ex3 = LETMREC
            (("even", "x",
              IF (EQUAL (VAR "x", CONST 0), TRUE,
                CALL (VAR "odd", SUB (VAR "x", CONST 1)))),
            ("odd", "x",
              IF (EQUAL (VAR "x", CONST 0), FALSE,
                CALL (VAR "even", SUB (VAR "x", CONST 1)))),
            CALL (VAR "odd", CONST 13)) in
print_endline (string_of_value (runml ex3));;

(* letrec factorial(x) =
 *          if (x = 0) then 1
 *          else factorial(x-1) * x
 * in letrec loop n =
 *      if (n = 0) then ()
 *      else (print (factorial n); loop (n-1))
 * in (loop 10)) 
 * 3628800
 * 362880
 * 40320
 * 5040
 * 720
 * 120
 * 24
 * 6
 * 2
 * 1 *)
let ex4 = LETREC ("factorial", "x",
            IF (EQUAL (VAR "x", CONST 0), CONST 1,
              MUL (CALL (VAR "factorial", SUB (VAR "x", CONST 1)), VAR "x")),
            LETREC ("loop", "n",
              IF (EQUAL (VAR "n", CONST 0), UNIT,
                SEQ (PRINT (CALL (VAR "factorial", VAR "n")),
                  CALL (VAR "loop", SUB (VAR "n", CONST 1)))),
              CALL (VAR "loop", CONST 10))) in
print_endline (string_of_value (runml ex4));;
