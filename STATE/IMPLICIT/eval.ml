open Lang
open Domain

let rec eval: exp -> (env * memory) -> (value * memory)
= fun exp (env, mem) ->
  match exp with
  | CONST n -> (Int n, mem)
  | VAR x -> (apply_mem (apply_env x env) mem, mem)
  | ADD (e1, e2) ->
    let v1, m1 = eval e1 (env, mem) in
    let v2, m2 = eval e2 (env, m1) in
      (match v1, v2 with
      | Int n1, Int n2 -> (Int (n1 + n2), m2)
      | _ -> raise (Failure "Type Error: non-numeric values"))
  | SUB (e1, e2) ->
    let v1, m1 = eval e1 (env, mem) in
    let v2, m2 = eval e2 (env, m1) in
      (match v1, v2 with
      | Int n1, Int n2 -> (Int (n1 - n2), m2)
      | _ -> raise (Failure "Type Error: non-numeric values"))
  | ISZERO e ->
    let v, m = eval e (env, mem) in 
    (match v with
    | Int n when n = 0 -> (Bool true, m)
    | Int n when n <> 0 -> (Bool false, m)
    | _ -> raise (Failure "Type Error: non-numeric values"))
  | IF (e1, e2, e3) ->
    let v, m = eval e1 (env, mem) in
    (match v with
    | Bool true -> eval e2 (env, m)
    | Bool false -> eval e3 (env, m)
    | _ -> raise (Failure "Type Error: condition must be Bool type"))
  | LET (x, e1, e2) -> 
    let v1, m1 = eval e1 (env, mem) in
    new_loc := !new_loc + 1;
    eval e2 ((extend_env (x, !new_loc) env), extend_mem (!new_loc, v1) m1)
  | LETREC (f, x, e1, e2) ->
    let rp = RecProc (f, x, e1, env) in
    new_loc := !new_loc + 1;
    eval e2 ((extend_env (f, !new_loc) env), extend_mem (!new_loc, rp) mem)
  | PROC (x, e) -> Proc (x, e, env), mem
  | APPLY (e1, e2) ->
    (match eval e1 (env, mem) with
    | Proc (x, e, env'), m1 -> 
      let v, m2 = eval e2 (env, m1) in
      new_loc := !new_loc + 1;
      eval e ((extend_env (x, !new_loc) env'), extend_mem (!new_loc, v) m2)
    | RecProc (_, x, e, _), m1 ->
      let v, m2 = eval e2 (env, m1) in
      new_loc := !new_loc + 1;
      eval e ((extend_env (x, !new_loc) env), extend_mem (!new_loc, v) m2)
    | _ -> raise (Failure "Type Error: APPLY must begin with an expression that implies a Proc or RecProc type object."))
  | APPLYREF (e1, y) ->
    (match eval e1 (env, mem) with
    | Proc (x, e, env'), m1 -> 
      eval e (extend_env (x, apply_env y env) env', m1)
    | _ -> raise (Failure "Type Error: APPLYREF must begin with an expression that implies a Proc type object."))
  | ASSIGN (x, e) ->
    let v, m1 = eval e (env, mem) in
    v, extend_mem(apply_env x env, v) m1
  | SEQ (e1, e2) ->
    let _, m1 = eval e1 (env, mem) in
    let v2, m2 = eval e2 (env, m1) in
    v2, m2
  | EMPTYREC -> Int 0, mem
  | RECORD (x, e1, y, e2) ->
    let v1, m1 = eval e1 (env, mem) in
    let v2, m2 = eval e2 (env, m1) in
    new_loc := !new_loc + 1;
    let l1 = !new_loc in
    new_loc := !new_loc + 1;
    let l2 = !new_loc in
    Record ((x, l1)::(y, l2)::[]), extend_mem (l2, v2) (extend_mem (l1, v1) m2)
  | FLD (e, x) ->
    (match eval e (env, mem) with
    | Record rcd, m1 -> apply_mem (apply_rec x rcd) m1, m1
    | _ -> raise (Failure "Type Error: FLD must begin with an expression that implies Record type object."))
  | ASSIGNFLD (e1, x, e2) ->
    (match eval e1 (env, mem) with
    | Record rcd, m1 -> 
      let v, m2 = eval e2 (env, m1) in
      v, extend_mem (apply_rec x rcd, v) m2
    | _ -> raise (Failure "Type Error: ASSIGNFLD must begin with an expression that implies Record type object."))
  | DEREF (x) ->
    Loc (apply_env x env), mem
  | DEREFFLD (e, x) ->
    (match eval e (env, mem) with
    | Record rcd, m1 -> Loc (apply_rec x rcd), m1
    | _ -> raise (Failure "Type Error: DEREFFLD must begin with an expression that implies Record type object."))
  | REF (e) ->
    (match eval e (env, mem) with
    | Loc l, m1 -> apply_mem l m1, m1
    | _ -> raise (Failure "Type Error: REF must begin with an expression that implies Loc type object."))
  | ASSIGNREF (e1, e2) ->
    (match eval e1 (env, mem) with
    | Loc l, m1 -> let v, m2 = eval e2 (env, m1) in v, extend_mem (l, v) m2
    | _ -> raise (Failure "Type Error: ASSIGNREF must begin with an expression that implies Loc type object."))
    

