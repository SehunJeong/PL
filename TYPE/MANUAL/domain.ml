open Lang

type value = 
  | Int of int
  | Bool of bool
  | Proc of proc
  | RecProc of recproc
and env = (var * value) list
and proc = var * exp * env
and recproc = var * var * exp * env

type tenv = (var * ttype) list

let string_of_value: value -> string
= fun v ->
  match v with
  | Int i -> string_of_int i
  | Bool true -> "true"
  | Bool false -> "false"
  | Proc _ -> "Procedure"
  | RecProc _ -> "RecProcedure"

let rec string_of_type: ttype -> string
= fun v ->
  match v with
  | TInt -> "TInt"
  | TBool -> "TBool"
  | TProc (t1, t2) -> string_of_type t1 ^ " -> " ^ string_of_type t2

let empty_env = []

let empty_tenv = []

let extend_env: (var * value) -> env -> env = fun (x,v) e -> 
  (x,v)::e

let extend_tenv: (var * ttype) -> tenv -> tenv = fun (x,t) e -> 
  (x,t)::e

let rec apply_env: var -> env -> value = fun x e -> 
  match e with
  | [] -> raise (Failure ("variable " ^ x ^ " not found"))
  | (y,v)::tl -> if x = y then v else apply_env x tl

let rec apply_tenv: var -> tenv -> ttype = fun x e -> 
  match e with
  | [] -> raise (Failure ("variable " ^ x ^ " not found"))
  | (y,t)::tl -> if x = y then t else apply_tenv x tl
