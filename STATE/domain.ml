open Lang

type value = 
  | Int of int
  | Bool of bool
  | Proc of var * exp * env
  | Loc of loc
and env = (var * value) list
and loc = int
and memory = (loc * value) list

let string_of_value: value -> string
= fun v ->
  match v with
  | Int i -> "Int " ^ string_of_int i
  | Bool true -> "Bool true"
  | Bool false -> "Bool false"
  | Proc _ -> "Procedure"
  | Loc l -> "Loc " ^ string_of_int l

let empty_env = []

let extend_env: (var * value) -> env -> env = fun (x,v) e -> 
  (x,v)::e

let rec apply_env: var -> env -> value = fun x e -> 
  match e with
  | [] -> raise (Failure ("variable " ^ x ^ " not found"))
  | (y,v)::tl -> if x = y then v else apply_env x tl
