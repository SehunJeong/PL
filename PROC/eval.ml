open Lang
open Domain

let rec eval: exp -> env -> value
= fun exp env ->
  match exp with
  | CONST n -> Int n
  | VAR x -> apply_env x env
  | ADD (e1, e2) ->
    let v1 = eval e1 env in
    let v2 = eval e2 env in
      (match v1, v2 with
      | Int n1, Int n2 -> Int (n1 + n2)
      | _ -> raise (Failure "Type Error: non-numeric values"))
  | SUB (e1, e2) ->
    let v1 = eval e1 env in
    let v2 = eval e2 env in
      (match v1, v2 with
      | Int n1, Int n2 -> Int (n1 - n2)
      | _ -> raise (Failure "Type Error: non-numeric values"))
  | READ -> Int (read_int())
  | ISZERO e -> 
    (match eval e env with
    | Int n when n = 0 -> Bool true
    | _ -> Bool false)
  | IF (e1, e2, e3) ->
    (match eval e1 env with
    | Bool true -> eval e2 env
    | Bool false -> eval e3 env
    | _ -> raise (Failure "Type Error: condition must be Bool type"))
  | LET (x, e1, e2) -> 
    let v1 = eval e1 env in
    eval e2 (extend_env (x, v1) env)
  | PROC (x, e) -> Proc (x, e, env)
  | LETREC (f, x, e1, e2) ->
    let rp = RecProc (f, x, e1, env) in
    eval e2 (extend_env (f, rp) env)
  | LETMREC (f, x1, f_e, g, x2, g_e, e) ->
    (*FIXME: *)
    let mrp = MRecProc (f, x1, f_e, g, x2, g_e, env) in
    eval e (extend_env (g, mrp) (extend_env (f, mrp) env))
    (*let rp1 = RecProc (f, x1, f_e, extend_env (g, RecProc(g, x2, g_e, env)) env) in
    let rp2 = RecProc (g, x2, g_e, extend_env (f, RecProc(f, x1, f_e, env)) env) in
    let env' = extend_env (f, rp1) env in
    let env'' = extend_env (g, rp2) env' in
    eval e env''   (* SEQ ("odd", 13) in env *) *)
  | SEQ (e1, e2) ->
    let v = eval e2 env in
    (match eval e1 env with
    | Proc (x, e, env') -> 
      eval e (extend_env (x, v) env')
    | RecProc (_, x, e, _) ->
      (*let rp = RecProc (f, x, e, env') in*)
      eval e (extend_env (x, v) env)
    | MRecProc (f, x1, f_e, g, x2, g_e, _) ->
      (match e1 with
      | VAR fg -> if fg = f then eval f_e (extend_env (x1, v) env) else (if fg = g then eval g_e (extend_env (x2, v) env) else raise (Failure ("Function not found")))
      | _ -> raise (Failure ("Function not found")))
    | _ -> raise (Failure "Type Error: Sequence must begin with an expression that implies a Proc, RecProc, or MRecProc type object.")) 
