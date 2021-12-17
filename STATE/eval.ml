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
  | ISZERO e -> 
    (match eval e env with
    | Int n when n = 0 -> Bool true
    | Int n when n <> 0 -> Bool false
    | _ -> raise (Failure "Type Error: non-numeric values"))
  | IF (e1, e2, e3) ->
    (match eval e1 env with
    | Bool true -> eval e2 env
    | Bool false -> eval e3 env
    | _ -> raise (Failure "Type Error: condition must be Bool type"))
  | LET (x, e1, e2) -> 
    let v1 = eval e1 env in
    eval e2 (extend_env (x, v1) env)
  | PROC (x, e) -> Proc (x, e, env)
  | APPLY (e1, e2) ->
    let v = eval e2 env in
    (match eval e1 env with
    | Proc (x, e, env') -> 
      eval e (extend_env (x, v) env')
    | _ -> raise (Failure "Type Error: APPLY must begin with an expression that implies a Proc type object."))
  | ALLOC (_) -> raise (Failure "Type Error: Undefined")
  | REF (_) -> raise (Failure "Type Error: Undefined")
  | ASSIGN (_, _) -> raise (Failure "Type Error: Undefined")
  | SEQ (_, _) -> raise (Failure "Type Error: Undefined")
