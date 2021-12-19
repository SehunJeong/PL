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

let string_of_memory: memory -> string
= fun m ->
  "List [" ^ (String.concat ";" (List.map (fun (l, v) -> string_of_int l ^ "->" ^ string_of_value v) m)) ^ "]"

let empty_env = []

let empty_mem = []

let extend_env: (var * value) -> env -> env = fun (x,v) e -> 
  (x,v)::e

let new_loc = ref 0;;

let extend_mem: (loc * value) -> memory -> memory = fun (l, v) mem ->
  (l, v)::mem

let rec apply_mem: loc -> memory -> value = fun l m ->
  match m with
  | [] -> raise (Failure ("location " ^ string_of_int l ^ " not found"))
  | (l', v)::tl -> if l = l' then v else apply_mem l tl

let rec apply_env: var -> env -> value = fun x e -> 
  match e with
  | [] -> raise (Failure ("variable " ^ x ^ " not found"))
  | (y,v)::tl -> if x = y then v else apply_env x tl
