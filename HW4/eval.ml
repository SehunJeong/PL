open Lang
open Domain

let rec eval: exp -> (env * memory) -> (value * memory)
= fun exp (env, mem) ->
  match exp with
  | TRUE -> Bool true, mem
  | FALSE -> Bool false, mem
  | NUM i -> Num i, mem
  | UNIT -> Unit, mem
  | VAR x -> 
    (match apply_env x env with
    | LocBind(_, b) -> apply_mem b mem, mem
    | _ -> raise (Failure ("Error: Evaluating VAR expression returns a non-location object")))
  | RECORD rs -> 
    if rs = [] then 
      Unit, mem 
    else (
      let acc_val_mem: (value list * memory) -> (id * exp) -> (value list * memory)
      = fun (vs, m) (_, e) -> let v, m' = eval e (env, m) in (v::vs, m') in 
      let values, m_n = List.fold_left acc_val_mem ([], mem) rs in
      let ids = List.map (fun (id, _) -> id) rs in
      let id_to_locs = List.map (fun id -> new_loc := !new_loc + 1; (id, !new_loc)) ids in
      let locs = List.map (fun (_, l) -> l) id_to_locs in
      let loc_to_values = List.map2 (fun l v -> (l, v)) locs values in
      Record id_to_locs, List.fold_left (fun m lv -> extend_mem lv m) m_n loc_to_values
    )
  | ADD (e1, e2) ->
    let v1, m' = eval e1 (env, mem) in
    let v2, m'' = eval e2 (env, m') in
    (match v1, v2 with
    | Num n1, Num n2 -> Num (n1 + n2), m''
    | _ -> raise (Failure "Type Error: non-numeric values"))
  | SUB (e1, e2) ->
    let v1, m' = eval e1 (env, mem) in
    let v2, m'' = eval e2 (env, m') in
    (match v1, v2 with
    | Num n1, Num n2 -> Num (n1 - n2), m''
    | _ -> raise (Failure "Type Error: non-numeric values"))
  | MUL (e1, e2) ->
    let v1, m' = eval e1 (env, mem) in
    let v2, m'' = eval e2 (env, m') in
    (match v1, v2 with
    | Num n1, Num n2 -> Num (n1 * n2), m''
    | _ -> raise (Failure "Type Error: non-numeric values"))
  | DIV (e1, e2) ->
    let v1, m' = eval e1 (env, mem) in
    let v2, m'' = eval e2 (env, m') in
    (match v1, v2 with
    | Num n1, Num n2 when n2 <> 0 -> Num (n1 + n2), m''
    | Num _, Num n2 when n2 = 0 -> raise (Failure "Undefined semantics: Division-by-zero.")
    | _ -> raise (Failure "Type Error: non-numeric values"))
  | _ -> raise (Failure "Unimplemented")