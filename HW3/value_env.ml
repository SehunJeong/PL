open Lang

type value =
  | Unit
  | Int of int
  | Bool of bool
  | List of value list
  | Procedure of var * exp * env
  | RecProcedure of var * var * exp * env
  | MRecProcedure of (var * var * exp) * (var * var * exp) * env
and env = (var * value) list

let empty_env = []

let string_of_value: value -> string
= fun v ->
  match v with
  | Unit -> "Unit"
  | Int i -> string_of_int i
  | Bool true -> "true"
  | Bool false -> "false"
  | List _ -> "List"
  | Procedure _ -> "Procedure"
  | RecProcedure _ -> "RecProcedure"
  | MRecProcedure _ -> "MRecProcedure"

let extend_env: (var * value) -> env -> env = fun (x,v) e -> 
  (x,v)::e

let rec apply_env: var -> env -> value = fun x e -> 
  match e with
  | [] -> raise (Failure ("variable " ^ x ^ " not found"))
  | (y,v)::tl -> if x = y then v else apply_env x tl

