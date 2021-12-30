open Domain
open Lang

exception TypeError of string

let rec typeof: tenv -> exp -> ttype
= fun tenv exp -> 
  match exp with
  | CONST _ -> TInt
  | VAR x -> apply_tenv x tenv
  | ADD (e1, e2) -> 
    (match (typeof tenv e1), (typeof tenv e2) with
    | TInt, TInt -> TInt
    | _ -> raise (TypeError "ADD requires integer-type variables"))
  | SUB (e1, e2) -> 
    (match (typeof tenv e1), (typeof tenv e2) with
    | TInt, TInt -> TInt
    | _ -> raise (TypeError "SUB requires integer-type variables"))
  | ISZERO (e) ->
    (match typeof tenv e with
    | TInt -> TBool
    | _ -> raise (TypeError "ISZERO requires integer-type variables"))
  | IF (e1, e2, e3) ->
    (match typeof tenv e1 with
    | TBool -> 
      let t1 = typeof tenv e2 in
      let t2 = typeof tenv e3 in
      if t1 = t2 then t1 else raise (TypeError "IF's \"else\" and \"then\" expressions should have same type")
    | _ -> raise (TypeError "IF's conditional expression should implies a Boolean-type variable"))
  | LET (x, e1, e2) ->
    let t1 = typeof tenv e1 in
    typeof (extend_tenv (x, t1) tenv) e2
  | APPLY (e1, e2) ->
    (match typeof tenv e1 with
    | TProc (t1, t2) when t1 = typeof tenv e2 -> t2
    | _ -> raise (TypeError "APPLY's formal and actual parameter types are inconsistent"))
  | PROC (x, t1, e) -> 
    TProc (t1, typeof (extend_tenv (x, t1) tenv) e)
  | LETREC (t1, f, x, t2, e1, e2) ->
    let t1' = typeof (extend_tenv (x, t2) (extend_tenv (f, TProc (t2, t1)) tenv)) e1 in
    if t1' = t1 then 
      typeof (extend_tenv (f, TProc (t2, t1)) tenv) e2 
    else raise (TypeError "LETREC's function body should have same type with the return type")
