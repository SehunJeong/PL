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
    | _ -> raise (UndefinedSemantics ("Error: Evaluating VAR expression returns a non-location object")))
  | RECORD rs ->
    (match rs with
    | [] -> Unit, mem
    | _ -> 
      let values, m_n = List.fold_left (fun (vs, m) (_, e) -> 
                                          let v, m' = eval e (env, m) in 
                                          (List.append vs [v], m')) 
                                       ([], mem) 
                                       rs in
      let id_to_locs = List.map (fun id -> new_loc := !new_loc + 1; (id, !new_loc)) (List.map (fun (id, _) -> id) rs) in
      let loc_to_values = List.map2 (fun l v -> (l, v)) (List.map (fun (_, l) -> l) id_to_locs) values in
      Record id_to_locs, List.fold_left (fun m lv -> extend_mem lv m) m_n loc_to_values)
  | ADD (e1, e2) ->
    let v1, m' = eval e1 (env, mem) in
    let v2, m'' = eval e2 (env, m') in
    (match v1, v2 with
    | Num n1, Num n2 -> Num (n1 + n2), m''
    | _ -> raise (UndefinedSemantics "Type Error: non-numeric values"))
  | SUB (e1, e2) ->
    let v1, m' = eval e1 (env, mem) in
    let v2, m'' = eval e2 (env, m') in
    (match v1, v2 with
    | Num n1, Num n2 -> Num (n1 - n2), m''
    | _ -> raise (UndefinedSemantics "Type Error: non-numeric values"))
  | MUL (e1, e2) ->
    let v1, m' = eval e1 (env, mem) in
    let v2, m'' = eval e2 (env, m') in
    (match v1, v2 with
    | Num n1, Num n2 -> Num (n1 * n2), m''
    | _ -> raise (UndefinedSemantics "Type Error: non-numeric values"))
  | DIV (e1, e2) ->
    let v1, m' = eval e1 (env, mem) in
    let v2, m'' = eval e2 (env, m') in
    (match v1, v2 with
    | Num n1, Num n2 when n2 <> 0 -> Num (n1 + n2), m''
    | Num _, Num n2 when n2 = 0 -> raise (UndefinedSemantics "Undefined semantics: Division-by-zero.")
    | _ -> raise (UndefinedSemantics "Type Error: non-numeric values"))
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
    | _ -> raise (UndefinedSemantics "Type Error: non-numeric values"))
  | NOT (e1) ->
    let v1, m' = eval e1 (env, mem) in
    (match v1 with
    | Bool b1 -> Bool (not b1), m'
    | _ -> raise (UndefinedSemantics "Type Error: non-boolean values"))
  | ASSIGN (id, e1) ->
    let v1, m' = eval e1 (env, mem) in
    (match apply_env id env with
    | LocBind (_, y) -> v1, extend_mem (y, v1) m'
    | _ -> raise (UndefinedSemantics "Type Error: l-value of ASSIGN should imply an identifier"))
  | ASSIGNF (e1, x, e2) -> 
    let r, m1 = eval e1 (env, mem) in
    (match r with
    | Record r -> 
      let v, m2 = eval e2 (env, m1) in
      v, extend_mem (apply_rec x r, v) m2
    | _ -> raise (UndefinedSemantics "Type Error: l-value of ASSIGNF should imply a record"))
  | FIELD (e1, id) ->
    let r, m' = eval e1 (env, mem) in
    (match r with
    | Record r -> 
      let (_, l) = List.find (fun (x, _) -> x = id) r in 
      apply_mem l m', m'
    | _ -> raise (UndefinedSemantics "Type Error: l-value of FIELD should imply a record"))
  | SEQ (e1, e2) ->
    let _, m' = eval e1 (env, mem) in
    let v2, m'' = eval e2 (env, m') in
    v2, m''
  | IF (e, e1, e2) -> 
    (match eval e (env, mem) with
    | Bool true, m' -> eval e1 (env, m')
    | Bool false, m' -> eval e2 (env, m')
    | _ -> raise (UndefinedSemantics "Type Error: non-boolean value"))
  | WHILE (e1, e2) ->
    (match eval e1 (env, mem) with
    | Bool true, m' -> 
      let _, m1 = eval e2 (env, m') in
      eval (WHILE (e1, e2)) (env, m1)
    | Bool false, m' -> Unit, m'
    | _ -> raise (UndefinedSemantics "Type Error: WHILE's first expression should imply boolean value"))
  | LETV (id, e1, e2) -> 
    let v, m' = eval e1 (env, mem) in
    new_loc := !new_loc + 1;
    eval e2 (extend_env (LocBind (id, !new_loc)) env, extend_mem (!new_loc, v) m')
  | LETF (f, args, e1, e2) ->
    eval e2 (extend_env (ProcBind (f, (args, e1, env))) env, mem)
  | CALLV (id, exps) ->
    (match apply_env id env with
    | ProcBind (f, proc) -> 
      let values, m_n = List.fold_left (fun (vs, m) e -> 
                                          let v, m' = eval e (env, m) in 
                                          (List.append vs [v], m')) 
                                        ([], mem) 
                                        exps in
      let (args, e', env') = proc in
      let id_to_locs = List.map (fun id -> 
                                  new_loc := !new_loc + 1; 
                                  (id, !new_loc)) 
                                args in
      let loc_to_values = List.map2 (fun l v -> (l, v)) (List.map (fun (_, l) -> l) id_to_locs) values in
      let env'' = List.fold_left (fun env (id, l) -> extend_env (LocBind (id, l)) env) env' (id_to_locs) in
      let env''' = extend_env (ProcBind (f, proc)) env'' in
      let mem'' = List.fold_left (fun mem loc_val -> extend_mem loc_val mem) m_n loc_to_values in
      eval e' (env''', mem'')
    | _ -> raise (UndefinedSemantics "Type Error: CALLV's first expression should imply a procedure"))
  | CALLR (f, ids) -> 
    (match apply_env f env with
    | ProcBind (f, proc) -> 
      let (args, e, env') = proc in
      let arg_to_locs = List.map2 (fun x y -> 
                                   (match apply_env y env with
                                   | LocBind (_, l) -> (x, l)
                                   | _ -> raise (UndefinedSemantics "Type Error: actual parameters should imply locations"))) args ids in
      let env'' = List.fold_left (fun env (id, loc) -> extend_env (LocBind (id, loc)) env) env' arg_to_locs in
      let env''' = extend_env (ProcBind (f, proc)) env'' in
      eval e (env''', mem)
    | _ -> raise (UndefinedSemantics "Type Error: CALLR's first expression should imply a procedure"))
  | WRITE (e) ->
    eval e (env, mem)