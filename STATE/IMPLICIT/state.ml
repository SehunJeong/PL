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

let counter = ref 0 in let f = fun _ -> counter := !counter + 1; !counter  in let a = (f 0) in let b = (f 0) in print_endline (string_of_int (a-b));;

let f = let counter = ref 0 in fun _ -> counter := !counter + 1; !counter  in let a = (f 0) in let b = (f 0) in print_endline (string_of_int (a-b));;

let f = fun _ -> let counter = ref 0 in counter := !counter + 1; !counter  in let a = (f 0) in let b = (f 0) in print_endline (string_of_int (a-b));;

(* let f = 
     let count = 0
     in proc (x) (set count = count + 1; count)
   in let a = (f 0)
      in let b = (f 0)
         in a - b *) (* -1 *)
let e11 = LET("f", 
            LET("count", CONST 0,
            PROC("x", SEQ(ASSIGN("count", ADD(VAR "count", CONST 1)), VAR "count"))),
          LET("a", APPLY(VAR "f", CONST 0),
            LET("b", APPLY(VAR "f", CONST 0),
              SUB(VAR "a", VAR "b")))) in
let v, m = run e11 in
print_endline (string_of_value v);
print_endline (string_of_memory m);;

(* letrec double(x) = 
 *   if iszero(x) then 0 else ((double (x-1)) + 1)
 * in (double 1) *) (* 2 *)
let e12 = LETREC("double", "x",
            IF(ISZERO(VAR "x"), CONST 0, ADD(APPLY(VAR "double", SUB(VAR "x", CONST 1)), CONST 2)),
            APPLY(VAR "double", CONST 1)) in
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