type program = exp
and exp = 
  | CONST of int
  | VAR of var
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | ISZERO of exp
  | READ
  | IF of exp * exp * exp
  | LET of var * exp * exp
  | LETREC of var * var * exp * exp
  | PROC of var * exp
  | CALL of exp * exp
and var = string

exception TypeError

type typ = TyInt | TyBool | TyFun of typ * typ | TyVar of tyvar
and tyvar = string
type typ_eqn = (typ * typ) list

let rec string_of_type ty = 
  match ty with
  | TyInt -> "int"
  | TyBool -> "bool"
  | TyFun (t1,t2) -> "(" ^ string_of_type t1 ^ " -> " ^ string_of_type t2 ^ ")"
  | TyVar x -> x

let print_typ_eqns eqns = 
  List.iter (fun (ty1,ty2) -> print_string (string_of_type ty1 ^ " = " ^ string_of_type ty2 ^ "\n")) eqns;
  print_endline ""

(* type environment : var -> type *)
module TEnv = struct
  type t = var -> typ
  let empty = fun _ -> raise (Failure "Type Env is empty")
  let extend (x,t) tenv = fun y -> if x = y then t else (tenv y)
  let find tenv x = tenv x
end

(* substitution *)
module Subst = struct
  type t = (tyvar * typ) list
  let empty = []
  let find x subst = List.assoc x subst

  (* walk through the type, replacing each type variable by its binding in the substitution *)
  let rec apply : typ -> t -> typ
  =fun typ subst ->
    match typ with
    | TyInt -> TyInt
    | TyBool -> TyBool 
    | TyFun (t1,t2) -> TyFun (apply t1 subst, apply t2 subst)
    | TyVar x -> 
      try find x subst
      with _ -> typ

  (* add a binding (tv,ty) to the subsutition and propagate the information *)
  let extend tv ty subst = 
    (tv,ty) :: (List.map (fun (x,t) -> (x, apply t [(tv,ty)])) subst)

  let print : t -> unit
  =fun subst -> 
      List.iter (fun (x,ty) -> print_endline (x ^ " |-> " ^ string_of_type ty)) subst
end

let tyvar_num = ref 0

(* generate a fresh type variable *)
let fresh_tyvar () = (tyvar_num := !tyvar_num + 1; (TyVar ("t" ^ string_of_int !tyvar_num)))

let rec gen_equations : TEnv.t -> exp -> typ -> typ_eqn 
=fun tenv e ty -> 
  match e with
  | CONST _ -> [(ty, TyInt)]
  | VAR x -> 
    (try [(ty, TEnv.find tenv x)] with _ -> [(ty, ty)])
  | ADD (e1, e2) | SUB (e1, e2) | MUL (e1, e2) | DIV (e1, e2) ->
    let eqns'' = gen_equations tenv e2 TyInt in
    let eqns' = gen_equations tenv e1 TyInt in
    (ty, TyInt)::(eqns'@eqns'')
  | ISZERO (e) -> 
    let eqns' = gen_equations tenv e TyInt in 
    (ty, TyBool)::eqns'
  | READ -> [(ty, TyInt)]
  | IF (e1, e2, e3) -> 
    let tv_for_e1 = fresh_tyvar () in
    let tv_for_e2 = fresh_tyvar () in
    let tv_for_e3 = fresh_tyvar () in
    let eqns' = gen_equations tenv e1 tv_for_e1 in
    let eqns'' = gen_equations tenv e2 tv_for_e2 in
    let eqns''' = gen_equations tenv e3 tv_for_e3 in
    [(tv_for_e2, ty); (tv_for_e3, ty)]@eqns'@eqns''@eqns'''
  | LET (x, e1, e2) -> 
    let tv_for_x = fresh_tyvar () in
    let tv_for_e1 = fresh_tyvar () in
    let tv_for_e2 = fresh_tyvar () in
    let eqns' = gen_equations tenv e1 tv_for_e1 in
    let tenv' = TEnv.extend (x, tv_for_x) tenv in
    let eqns'' = gen_equations tenv' e2 tv_for_e2 in
    [(tv_for_x, tv_for_e1); (tv_for_e2, ty)]@eqns'@eqns''
  | LETREC (f, x, e1, e2) -> 
    let tv_for_f = fresh_tyvar () in
    let tv_for_x = fresh_tyvar () in 
    let tv_for_e1 = fresh_tyvar () in 
    let tenv' = TEnv.extend (f, TyFun(tv_for_x, tv_for_f)) (TEnv.extend (x, tv_for_x) tenv) in 
    let eqns' = gen_equations tenv' e1 tv_for_e1 in 
    let tenv'' = TEnv.extend (f, TyFun(tv_for_x, tv_for_f)) tenv in
    let eqns'' = gen_equations tenv'' e2 ty in 
    eqns'@eqns''
  | PROC (x, e) -> 
    let tv_for_x = fresh_tyvar () in
    let tv_for_e = fresh_tyvar () in
    let tenv' = TEnv.extend (x, tv_for_x) tenv in
    (ty, TyFun (tv_for_x, tv_for_e))::gen_equations tenv' e tv_for_e
  | CALL (f, e) ->
    (match f with
    | VAR f -> 
      let tv_for_f = TEnv.find tenv f in
      let tv_for_e = fresh_tyvar () in
      (tv_for_f, TyFun(tv_for_e, ty))::gen_equations tenv e tv_for_e
    | _ -> raise TypeError)

let rec is_contradict : typ -> typ -> bool
=fun t1 t2 ->
  match (t1, t2) with
  | (TyVar _, _) | (_, TyVar _) -> false
  | (TyFun (x, y), TyFun (x', y')) -> (is_contradict x x' || is_contradict y y')
  | (TyFun _, _) | (_, TyFun _) -> true
  | (TyBool, TyInt) | (TyInt, TyBool) -> true
  | (TyBool, TyBool) | (TyInt, TyInt) -> false
  

let solve : typ_eqn -> Subst.t
=fun eqns -> 
  let rec simplifying : typ_eqn -> typ_eqn
  =fun eqns ->
    List.concat_map (fun eqn -> 
      match eqn with
      | (TyFun (t1, t2), TyFun (t1', t2')) -> simplifying [(t1, t1')]@simplifying [(t2, t2')]
      | _ -> [eqn]) eqns 
  in 
  let switch_and_check : (typ * typ) -> (typ * typ)
  =fun (t1, t2) ->
    (t1, t2)
  in
  let solve_eqn : Subst.t -> (typ * typ) -> Subst.t
  =fun subst (t1, t2) ->
    let (t1', t2') = (Subst.apply t1 subst, Subst.apply t2 subst) in
    if t1' = t2' then subst 
    else begin 
      if is_contradict t1' t2' then raise TypeError
      else begin
        let (t1'', t2'') = switch_and_check (t1', t2') in
        subst
      end 
    end
  in
  let eqns' = simplifying eqns in 
  List.fold_left solve_eqn Subst.empty eqns'

(* typeof: Do not modify this function *)
let typeof : exp -> typ 
=fun exp ->
  let new_tv = fresh_tyvar () in
  let eqns = gen_equations TEnv.empty exp new_tv in
  let _ = print_endline "= Equations = ";
          print_typ_eqns eqns in
  try 
    let subst = solve eqns in
    let ty = Subst.apply new_tv subst in
     print_endline "= Substitution = ";
      Subst.print subst;
      print_endline "";
      print_endline ("Type of the given program: " ^ string_of_type ty);
      print_endline "";
      ty
  with TypeError -> (print_endline "The program does not have type. Rejected."); exit (1)
