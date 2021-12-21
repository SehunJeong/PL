open Lang
open Domain

let rec eval: exp -> (env * memory) -> (value * memory)
= fun exp (env, mem) ->
  match exp with
  | CONST n -> (Int n, mem)
  | VAR x -> (apply_env x env, mem)
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
    let v1, m = eval e1 (env, mem) in
    eval e2 ((extend_env (x, v1) env), m)
  | PROC (x, e) -> Proc (x, e, env), mem
  | APPLY (e1, e2) ->
    (match eval e1 (env, mem) with
    | Proc (x, e, env'), m1 -> 
      let v, m2 = eval e2 (env, m1) in
      eval e ((extend_env (x, v) env'), m2)
    | _ -> raise (Failure "Type Error: APPLY must begin with an expression that implies a Proc type object."))
  | ASSIGN (e1, e2) ->
    (match eval e1 (env, mem) with
    | Loc l, m1 -> 
      let v, m2 = eval e2 (env, m1) in
      v, extend_mem (l, v) m2
    | _ -> raise (Failure "Type Error: not a location"))
  | SEQ (e1, e2) ->
    let _, m1 = eval e1 (env, mem) in
    let v2, m2 = eval e2 (env, m1) in
    v2, m2
