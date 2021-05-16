open Lang
open Val
open Env
open Eval

let run: program -> value = fun pgm -> eval pgm empty_env
let v = run (VAR "X");;
() = print_endline "Hello, World!";
