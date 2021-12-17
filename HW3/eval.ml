open Lang
open Value_env

let rec eval: exp -> env -> value
= fun exp env ->
  match exp with
  | UNIT ->
    Unit
  | TRUE -> Bool true
  | FALSE -> Bool false
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
  | MUL (e1, e2) ->
    (match eval e1 env, eval e2 env with
    | Int n1, Int n2 -> Int (n1 * n2)
    | _ -> raise (UndefinedSemantics "Type Error: non-numeric values"))
  | DIV (_, _) -> raise (UndefinedSemantics "DIV: Undefined")
  | EQUAL (e1, e2) ->
    (match eval e1 env, eval e2 env with
    | Bool b1, Bool b2 -> if b1 = b2 then Bool true else Bool false
    | Int n1, Int n2 -> if n1 = n2 then Bool true else Bool false
    | _ -> raise (UndefinedSemantics "Type Error: equality is defined for integers and booleans"))
  | LESS (_, _) -> raise (UndefinedSemantics "LESS: Undefined")
  | NOT (e) -> 
    (match eval e env with
    | Bool true -> Bool false
    | Bool false -> Bool true
    | _ -> raise (UndefinedSemantics "Type Error: NOT is defined for booleans"))
  | NIL -> List []
  | CONS (e1, e2) -> 
    let v = eval e1 env in
    (match eval e2 env with
    | List l when l = [] -> List ([v])
    | List l when l <> [] -> List (List.cons v l)
    | _ -> raise (UndefinedSemantics ""))
  | APPEND (_, _) -> raise (UndefinedSemantics "APPEND: Undefined")
  | HEAD (_) -> raise (UndefinedSemantics "HEAD: Undefined")
  | TAIL (_) -> raise (UndefinedSemantics "TAIL: Undefined")
  | ISNIL (_) -> raise (UndefinedSemantics "ISNIL: Undefined")
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
  | LETMREC ((f, x1, f_e), (g, x2, g_e), e) ->
    let mrp = MRecProcedure ((f, x1, f_e), (g, x2, g_e), env) in
    eval e (extend_env (g, mrp) (extend_env (f, mrp) env))
  | PROC (x, e) -> Procedure (x, e, env)
  | CALL (e1, e2) ->
    let v = eval e2 env in
    (match eval e1 env with
    | Procedure (x, e, env') -> 
      eval e (extend_env (x, v) env')
    | RecProcedure (_, x, e, _) ->
      eval e (extend_env (x, v) env)
    | MRecProcedure ((f, x1, f_e), (g, x2, g_e), _) ->
      (match e1 with
      | VAR x when x = f ->
        eval f_e (extend_env (x1, v) env) 
      | VAR x when x = g ->
        eval g_e (extend_env (x2, v) env) 
      | _ -> raise (UndefinedSemantics ("MRecProcedure not found")))
    | _ -> raise (UndefinedSemantics "Type Error: CALL must begin with an expression that implies a Procedure, RecProcedure, or MRecProcedure type object."))
  | PRINT (e) ->
    print_endline (string_of_value (eval e env));
    Unit
  | SEQ (e1,e2) ->
    let _ = eval e1 env in
    eval e2 env
