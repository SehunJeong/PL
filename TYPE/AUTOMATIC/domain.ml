open Lang

let rec string_of_type: typ -> string
= fun v ->
  match v with
  | TyInt -> "TyInt"
  | TyBool -> "TyBool"
  | TyFun (t1, t2) -> string_of_type t1 ^ " -> " ^ string_of_type t2
  | TyVar tyvar -> tyvar