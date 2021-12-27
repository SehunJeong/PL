open Domain
open Eval
open Lang

let runb: exp -> value
= fun pgm -> 
    let v, _ = eval pgm (empty_env, empty_mem) in
    v;;

(* let ret = 1 in
   let n = 5 in
   while (0 < n) {
      ret := ret * n;
      n := n - 1;
   };
   ret *) 
(* 120 *)
let e1 = LETV ("ret", NUM 1,
            LETV ("n", NUM 5,
                SEQ (
                    WHILE (LESS (NUM 0, VAR "n"),
                        SEQ (
                            ASSIGN ("ret", MUL (VAR "ret", VAR "n")),
                            ASSIGN ("n", SUB (VAR "n", NUM 1))
                        )
                    ),
                    VAR "ret"))) in
let v = runb e1 in
print_endline (string_of_value v);;

(* let proc f (x1, x2) =
     x1 := 3;
     x2 := 3;
   in
   let x1 = 1 in
   let x2 = 1 in
   f <x1, x2>;
   x1 + x2 *)
(* 6 *)
let e2 = LETF ("f", ["x1"; "x2"],
            SEQ (
                ASSIGN ("x1", NUM 3),
                ASSIGN ("x2", NUM 3)
            ),
            LETV("x1", NUM 1,
                LETV("x2", NUM 1,
                    SEQ(
                        CALLR ("f", ["x1"; "x2"]),
                        ADD(VAR "x1", VAR "x2"))))) in 
let v = runb e2 in
print_endline (string_of_value v);;

(* let f = {x := 10, y := 13} in
    let proc swap (a, b) =
        let temp = a in
        a := b;
        b := temp
    in
    swap (f.x, f.y);
    f.x *)
(* 10 *)
let e3 = LETV ("f", RECORD ([("x", NUM 10); ("y", NUM 13)]),
            LETF ("swap", ["a"; "b"],
                LETV ("temp", VAR "a",
                    SEQ (
                        ASSIGN ("a", VAR "b"),
                        ASSIGN ("b", VAR "temp"))),
                SEQ (
                    CALLV("swap", [FIELD (VAR "f", "x"); FIELD (VAR "f", "y")]),
                    FIELD (VAR "f", "x")
                )
            )
        ) in
let v = runb e3 in
print_endline (string_of_value v);;