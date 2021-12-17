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

(* letrec range(n) =
 *        if (n = 1) then (cons 1 nil)
 *        else n::(range (n-1))
 *  in (range 10)
 * 
  * List [Int 10; Int 9; Int 8; Int 7; Int 6; Int 5; Int 4; Int 3; Int 2; Int 1] *)
let ex5 = LETREC ("range", "n",
            IF (EQUAL (VAR "n", CONST 1), CONS (CONST 1, NIL),
              CONS (VAR "n", CALL (VAR "range", SUB (VAR "n", CONST 1)))),
            CALL (VAR "range", CONST 10)) in
print_endline (string_of_value (runml ex5));;

(* letrec reverse(l) =
     if (isnil l) then []
     else (reverse (tl l)) @ (cons hd l)
   in (reverse (cons (1, cons (2, cons (3, nil))))) 
   
   List [Int 3; Int 2; Int 1].*)
let ex6 = LETREC ("reverse", "l",
            IF (ISNIL (VAR "l"), NIL,
              APPEND (CALL (VAR "reverse", TAIL (VAR "l")), CONS (HEAD (VAR "l"), NIL))),
            CALL (VAR "reverse", CONS (CONST 1, CONS (CONST 2, CONS (CONST 3, NIL))))) in
print_endline (string_of_value (runml ex6));;

(* let fix = proc (f) ((proc (x) f (proc (y) ((x x) y)))
                       (proc (x) f (proc (y) ((x x) y))))
   in let f = fix (proc (f) (proc (x) (if (x = 0) then 1 else f(x-1) * x)))
      in (f 10)
Int 3628800*)
let ex7_1 = LET ("fix",
              PROC ("f",
                CALL
                  (PROC ("x",
                    CALL (VAR "f", PROC ("y", CALL (CALL (VAR "x", VAR "x"), VAR "y")))),
                  PROC ("x",
                    CALL (VAR "f", PROC ("y", CALL (CALL (VAR "x", VAR "x"), VAR "y")))))),
              LET ("f",
                CALL (VAR "fix",
                  PROC ("f",
                    PROC ("x",
                      IF (EQUAL (VAR "x", CONST 0), CONST 1,
                        MUL (CALL (VAR "f", SUB (VAR "x", CONST 1)), VAR "x"))))),
                 CALL (VAR "f", CONST 10))) in
print_endline (string_of_value (runml ex7_1));;

(* let fix = proc (f) ((proc (x) f (proc (y) ((x x) y)))
                       (proc (x) f (proc (y) ((x x) y))))
   in let f = fix (proc (range)
                    (proc (n)
                      (if (n = 1) then (cons 1 nil)
                      else n::(range (n-1)))))
      in (f 10)
List [Int 10; Int 9; Int 8; Int 7; Int 6; Int 5; Int 4; Int 3; Int 2; Int 1]*)
let ex7_2 = LET ("fix",
              PROC ("f",
                CALL
                  (PROC ("x",
                    CALL (VAR "f", PROC ("y", CALL (CALL (VAR "x", VAR "x"), VAR "y")))),
                  PROC ("x",
                    CALL (VAR "f", PROC ("y", CALL (CALL (VAR "x", VAR "x"), VAR "y")))))),
              LET ("f",
                CALL (VAR "fix",
                  PROC ("range",
                    PROC ("n",
                      IF (EQUAL (VAR "n", CONST 1), CONS (CONST 1, NIL),
                        CONS (VAR "n", CALL (VAR "range", SUB (VAR "n", CONST 1))))))),
                CALL (VAR "f", CONST 10))) in
print_endline (string_of_value (runml ex7_2));