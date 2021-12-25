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
    (match rs with
    | [] -> Unit, mem
    | _ -> 
      let values, m_n = List.fold_left (fun (vs, m) (_, e) -> let v, m' = eval e (env, m) in (v::vs, m')) ([], mem) rs in
      let id_to_locs = List.map (fun id -> new_loc := !new_loc + 1; (id, !new_loc)) (List.map (fun (id, _) -> id) rs) in
      let loc_to_values = List.map2 (fun l v -> (l, v)) (List.map (fun (_, l) -> l) id_to_locs) values in
      Record id_to_locs, List.fold_left (fun m lv -> extend_mem lv m) m_n loc_to_values)
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
  | EQUAL (e1, e2) ->
    let v1, m' = eval e1 (env, mem) in
    let v2, m'' = eval e2 (env, m') in
    (match v1, v2 with
    | Num n1, Num n2 when n1 = n2 -> Bool true, m''
    | Bool b1, Bool b2 when b1 = b2 -> Bool true, m''
    | Unit, Unit -> Bool true, m''
    | _, _ -> Bool false, m'')
  | LESS (e1, e2) ->
    let v1, m' = eval e1 (env, mem) in
    let v2, m'' = eval e2 (env, m') in
    (match v1, v2 with
    | Num n1, Num n2 -> Bool (n1 < n2), m''
    | _ -> raise (Failure "Type Error: non-numeric values"))
  | NOT (e1) ->
    let v1, m' = eval e1 (env, mem) in
    (match v1 with
    | Bool b1 -> Bool (not b1), m'
    | _ -> raise (Failure "Type Error: non-boolean values"))
  | ASSIGN (id, e1) ->
    let v1, m' = eval e1 (env, mem) in
    (match apply_env id env with
    | LocBind (_, y) -> v1, extend_mem (y, v1) m'
    | _ -> raise (Failure "Type Error: l-value of ASSIGN should imply an identifier"))
  | ASSIGNF _ -> raise (Failure "Unimplemented")
  | FIELD (e1, id) ->
    let r, m' = eval e1 (env, mem) in
    (match r with
    | Record r -> 
      let (_, l) = List.find (fun (x, _) -> x = id) r in 
      apply_mem l m', m'
    | _ -> raise (Failure "Type Error: l-value of FIELD should imply a record"))
  | SEQ (e1, e2) ->
    let _, m' = eval e1 (env, mem) in
    let v2, m'' = eval e2 (env, m') in
    v2, m''
  | IF (e, e1, e2) -> 
    (match eval e (env, mem) with
    | Bool true, m' -> eval e1 (env, m')
    | Bool false, m' -> eval e2 (env, m')
    | _ -> raise (Failure "Type Error: non-boolean value"))
  | WHILE (e1, e2) ->
    (match eval e1 (env, mem) with
    | Bool true, m' -> 
      let _, m1 = eval e2 (env, m') in
      eval (WHILE (e1, e2)) (env, m1)
    | Bool false, m' -> Unit, m'
    | _ -> raise (Failure "Type Error: non-boolean value"))
  | LETF _ -> raise (Failure "Unimplemented")
  | LETV _ -> raise (Failure "Unimplemented")
  | CALLR _ -> raise (Failure "Unimplemented")
  | CALLV _ -> raise (Failure "Unimplemented")
  | WRITE _ -> raise (Failure "Unimplemented")