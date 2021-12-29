open Lang

type loc = int
type value =
| Num of int
| Bool of bool
| Unit
| Record of record
and record = (id * loc) list
type memory = (loc * value) list
type env = binding list
and binding = LocBind of id * loc | ProcBind of id * proc
and proc = id list * exp * env

exception UndefinedSemantics of string

let string_of_value: value -> string
= fun v ->
  match v with
  | Num i -> "Int " ^ string_of_int i
  | Bool true -> "Bool true"
  | Bool false -> "Bool false"
  | Unit -> "Unit"
  | Record _ -> "Record"

let empty_env = []

let empty_mem = []

let new_loc = ref 0;;

let extend_mem: (loc * value) -> memory -> memory = fun (l, v) mem ->
  (l, v)::mem

let extend_env: binding -> env -> env = fun b e -> 
  b::e

let rec apply_env: id -> env -> binding
= fun v env ->
  match env with
  | [] -> raise (UndefinedSemantics ("Error: Variable " ^ v ^ " is not found."))
  | LocBind (x, y)::tl -> if x = v then LocBind (x, y) else apply_env v tl
  | ProcBind (x, y)::tl -> if x = v then ProcBind (x, y) else apply_env v tl

let rec apply_mem: loc -> memory -> value
= fun l mem ->
  match mem with
  | [] -> raise (UndefinedSemantics ("Error: location " ^ string_of_int l ^ " is not found."))
  | (x, y)::tl -> if x = l then y else apply_mem l tl

let rec apply_rec: id -> record -> loc
= fun x r ->
  match r with
  | [] -> raise (UndefinedSemantics ("Error: Field " ^ x ^ " is not found."))
  | (y, l)::tl -> if x = y then l else apply_rec x tl

let string_of_memory: memory -> string
= fun m ->
  "List [" ^ (String.concat ";" (List.map (fun (l, v) -> string_of_int l ^ "->" ^ string_of_value v) m)) ^ "]"

let rec reach: env -> memory -> loc list
= fun env mem ->
  let domains = List.map 
                  (fun b -> match b with 
                            | LocBind (x, _) -> x
                            | ProcBind (x, _) -> x
                  ) env in  
  let tmp = List.fold_left 
              (fun r id -> match apply_env id env with
                           | LocBind (_, l) -> l::r
                           | ProcBind (_, (_, _, env')) -> List.append r (reach env' mem)
              ) [] domains in
  let rec dig_loc: loc -> loc list
  = fun l ->
    match apply_mem l mem with
    | Record id_loc_list -> 
      List.fold_left 
        (fun locs (_, loc) -> 
          List.append locs (dig_loc loc)
        ) [l] id_loc_list
    | _ -> [l] in
  List.concat_map (fun loc -> dig_loc loc) tmp

let gc: env -> memory -> memory
= fun env mem ->
  let r = reach env mem in
  List.filter (fun (l, _) -> List.mem l r) mem