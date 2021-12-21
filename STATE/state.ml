open Domain
open Eval
open Lang

let run: program -> (value * memory)
= fun pgm -> 
    eval pgm (empty_env, empty_mem);;

(* let x = 1 in 
   x + 2 *) 
(* 3 *)
let e1 = LET ("x", CONST 1, 
           ADD (VAR "x", CONST 2)) in
let v, m = run e1 in
print_endline (string_of_value v);
print_endline (string_of_memory m);;

(* let x = 1 in 
   let y = 2 in 
   x + y *) 
(* 3 *)
let e2 = LET ("x", CONST 1, 
           LET ("y", CONST 2, 
             ADD (VAR "x", VAR "y"))) in
let v, m = run e2 in
print_endline (string_of_value v);
print_endline (string_of_memory m);;

(* let x = 7 
 * in let y = 2 
 * in let y = let x = x - 1 
 *            in x - y 
 * in (x-8)-y *) 
 (* -5 *)
let e3 = LET ("x", CONST 7, 
           LET ("y", CONST 2, 
             LET ("y", 
               LET ("x", SUB (VAR "x", CONST 1), 
                 SUB (VAR "x", VAR "y")), 
               SUB (SUB (VAR "x", CONST 8), VAR "y")))) in
let v, m = run e3 in
print_endline (string_of_value v);
print_endline (string_of_memory m);;

(* let x = let y = 2 
 *         in y + 1 
 * in x + 3 *) (* 6 *)
 let e5 = LET ("x", 
            LET ("y", CONST 2, 
              ADD (VAR "y", CONST 1)), 
            ADD (VAR "x", CONST 3)) in
 let v, m = run e5 in
print_endline (string_of_value v);
print_endline (string_of_memory m);;

(* let x = 1 
 * in let y = 2 
 * in let x = 3 
 * in x + y *) (* 5 *)
 let e6 = LET ("x", CONST 1, 
            LET ("y", CONST 2, 
              LET ("x", CONST 3, 
                ADD (VAR "x", VAR "y")))) in
 let v, m = run e6 in
print_endline (string_of_value v);
print_endline (string_of_memory m);;

(* let x = 1 
 * in let y = let x = 2 
 *            in x + x 
 * in x + y *) (* 5 *)
 let e7 = LET ("x", CONST 1, 
            LET ("y", 
              LET ("x", CONST 2, 
                ADD (VAR "x", VAR "x")), 
            ADD (VAR "x", VAR "y"))) in
 let v, m = run e7 in
print_endline (string_of_value v);
print_endline (string_of_memory m);;

(* let x = 1 
 * in let y = 2 
 * in if iszero (x - 1) then 
 *   y - 1 
 * else 
 *   y + 1 *) (* 1 *)
 let e8 = LET ("x", CONST 1, 
            LET ("y", CONST 2, 
              IF (ISZERO (SUB (VAR "x", CONST 1)), 
                SUB (VAR "y", CONST 1), 
                ADD (VAR "y", CONST 1)))) in
 let v, m = run e8 in
print_endline (string_of_value v);
print_endline (string_of_memory m);;

(* (proc (x) (x)) 1 *) (* 1 *)
let e9 = APPLY(PROC ("x", VAR "x"), CONST 1) in
let v, m = run e9 in
print_endline (string_of_value v);
print_endline (string_of_memory m);;

(* let x = 1
 * in let f = proc (y) (x+y)
 *    in let x = 2
 *       in (f 3) *) (* 4 *)
let e10 = LET("x", CONST 1, 
            LET ("f", PROC ("y", ADD(VAR "x", VAR "y")), 
              LET ("x", CONST 2, APPLY(VAR "f", CONST 3)))) in
let v, m = run e10 in
print_endline (string_of_value v);
print_endline (string_of_memory m);;

(* let x = ref (ref 0)
   in (!x := 11; !(!x))
   11
*)
let e11 = LET("x", ALLOC(ALLOC (CONST 0)), 
            SEQ(ASSIGN(REF(VAR "x"), CONST 11), 
                REF(REF(VAR "x")))) in
let v, m = run e11 in
print_endline (string_of_value v);
print_endline (string_of_memory m);;

(* let cnt = ref 0
   in let rec is_zero(x) = cnt := !cnt + 1;
                           if x == 0 then true
                           else is_zero(x-1) in
        is_zero (3)
  Int 1 [x -> Int 4; x -> 3; ...]*)
let e12 = LET("cnt", ALLOC (CONST 0), 
            LETREC("call_is_zero", 
                   "x", 
                   SEQ(ASSIGN(VAR "cnt", ADD(REF (VAR "cnt"), CONST 1)),
                       IF(ISZERO(VAR "x"), 
                         CONST 1, 
                         APPLY(VAR "call_is_zero", SUB(VAR "x", CONST 1)))),
                   APPLY(VAR "call_is_zero", CONST 3))) in
let v, m = run e12 in
print_endline (string_of_value v);
print_endline (string_of_memory m);;
          

(* let x = 1 in 
     let y = iszero x in 
       x + y *) (* exception *)
let e4 = LET ("x", CONST 1, 
           LET ("y", ISZERO (VAR "x"), 
             ADD (VAR "x", VAR "y"))) in
let v, m = run e4 in
print_endline (string_of_value v);
print_endline (string_of_memory m);;