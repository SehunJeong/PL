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
    let mrp = MRecProc (f, x1, f_e, g, x2, g_e, env) in
    eval e (extend_env (g, mrp) (extend_env (f, mrp) env))
  | APPLY (e1, e2) ->
    let v = eval e2 env in
    (match eval e1 env with
    | Proc (x, e, env') -> 
      eval e (extend_env (x, v) env')
    | RecProc (_, x, e, _) ->
      eval e (extend_env (x, v) env)
    | MRecProc (f, x1, f_e, g, x2, g_e, _) ->
      (match e1 with
      | VAR x when x = f ->
        eval f_e (extend_env (x1, v) env) 
      | VAR x when x = g ->
        eval g_e (extend_env (x2, v) env) 
      | _ -> raise (Failure ("Function not found")))
    | _ -> raise (Failure "Type Error: APPLY must begin with an expression that implies a Proc, RecProc, or MRecProc type object."))
