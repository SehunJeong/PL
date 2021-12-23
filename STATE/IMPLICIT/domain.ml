open Lang

type value = 
  | Int of int
  | Bool of bool
  | Proc of var * exp * env
  | RecProc of var * var * exp * env
  | Loc of loc
  | Record of rcd
and rcd = (var * loc) list  
and env = (var * loc) list
and loc = int
and memory = (loc * value) list

let string_of_value: value -> string
= fun v ->
  match v with
  | Int i -> "Int " ^ string_of_int i
  | Bool true -> "Bool true"
  | Bool false -> "Bool false"
  | Proc _ -> "Procedure"
  | RecProc _ -> "RecProcedure"
  | Loc l -> "Loc " ^ string_of_int l
  | Record _ -> "Record"

let string_of_memory: memory -> string
= fun m ->
  "List [" ^ (String.concat ";" (List.map (fun (l, v) -> string_of_int l ^ "->" ^ string_of_value v) m)) ^ "]"

let empty_env = []

let empty_mem = []

let extend_env: (var * loc) -> env -> env = fun (x,l) e -> 
  (x,l)::e

let new_loc = ref 0;;

let extend_mem: (loc * value) -> memory -> memory = fun (l, v) mem ->
  (l, v)::mem

let rec apply_mem: loc -> memory -> value = fun l m ->
  match m with
  | [] -> raise (Failure ("location " ^ string_of_int l ^ " not found"))
  | (l', v)::tl -> if l = l' then v else apply_mem l tl

let rec apply_env: var -> env -> loc = fun x e -> 
  match e with
  | [] -> raise (Failure ("variable " ^ x ^ " not found"))
  | (y,l)::tl -> if x = y then l else apply_env x tl

let rec apply_rec: var -> rcd -> loc = fun x r ->
  match r with 
  | [] -> raise (Failure ("field " ^ x ^ " not found"))
  | (y, l)::tl -> if x = y then l else apply_rec x tl