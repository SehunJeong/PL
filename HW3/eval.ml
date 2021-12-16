open Lang
open Value_env

let rec eval: exp -> env -> value
= fun exp env ->
  match exp with
  | CONST n -> Int n
  | VAR x -> apply_env x env
  | ADD (e1, e2) ->
    (match eval e1 env, eval e2 env with
    | Int n1, Int n2 -> Int (n1 + n2)
    | _ -> raise (UndefinedSemantics "Type Error: non-numeric values"))
  | SUB (e1, e2) ->
    (match eval e1 env, eval e2 env with
    | Int n1, Int n2 -> Int (n1 - n2)
    | _ -> raise (UndefinedSemantics "Type Error: non-numeric values"))
  | EQUAL (e1, e2) ->
    (match eval e1 env, eval e2 env with
    | Bool b1, Bool b2 -> if b1 = b2 then Bool true else Bool false
    | Int n1, Int n2 -> if n1 = n2 then Bool true else Bool false
    | _ -> raise (UndefinedSemantics "Type Error: equality is defined for integers and booleans"))
  | IF (e, e_true, e_false) ->
    (match eval e env with
    | Bool true -> eval e_true env
    | Bool false -> eval e_false env
    | _ -> raise (UndefinedSemantics "Type Error: condition must be evaluated as Bool type"))
  | LET (x, e1, e2) -> 
    eval e2 (extend_env (x, eval e1 env) env)
  | LETREC (f, x, e1, e2) ->
    let rp = RecProcedure (f, x, e1, env) in
    eval e2 (extend_env (f, rp) env)
  | PROC (x, e) -> Procedure (x, e, env)
  | CALL (e1, e2) ->
    let v = eval e2 env in
    (match eval e1 env with
    | Procedure (x, e, env') -> 
      eval e (extend_env (x, v) env')
    | RecProcedure (_, x, e, _) ->
      eval e (extend_env (x, v) env)
    | _ -> raise (Failure "Type Error: CALL must begin with an expression that implies a Procedure, RecProcedure, or MRecProcedure type object."))
  | _ -> raise (UndefinedSemantics "Type Error")
