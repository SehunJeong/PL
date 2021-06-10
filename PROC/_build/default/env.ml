open Val
open Lang

type env = (var * value) list
let empty_env = []
let extend_env: (var * value) -> env -> env = fun (x,v) e -> 
  (x,v)::e
let rec apply_env: var -> env -> value = fun x e -> 
  match e with
  | [] -> raise (Failure ("variable " ^ x ^ " not found"))
  | (y,v)::tl -> if x = y then v else apply_env x tl
