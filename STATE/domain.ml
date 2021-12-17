open Lang

type value = 
  | Int of int
  | Bool of bool
  | Proc of proc
  | RecProc of recproc
and env = (var * value) list
and proc = var * exp * env
and recproc = var * var * exp * env

let string_of_value: value -> string
= fun v ->
  match v with
  | Int i -> string_of_int i
  | Bool true -> "true"
  | Bool false -> "false"
  | Proc _ -> "Procedure"
  | RecProc _ -> "RecProcedure"

let empty_env = []

let extend_env: (var * value) -> env -> env = fun (x,v) e -> 
  (x,v)::e

let rec apply_env: var -> env -> value = fun x e -> 
  match e with
  | [] -> raise (Failure ("variable " ^ x ^ " not found"))
  | (y,v)::tl -> if x = y then v else apply_env x tl
