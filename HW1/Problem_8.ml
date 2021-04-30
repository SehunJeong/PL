type formula = 
  | True
  | False
  | Not of formula
  | AndAlso of formula * formula
  | OrElse of formula * formula
  | Imply of formula * formula
  | Equal of exp * exp 
and exp = 
  | Num of int
  | Plus of exp * exp
  | Minus of exp * exp;;

let rec eval_exp : exp -> int = fun e ->
  match e with
  | Num i -> i
  | Plus (e1, e2) -> (eval_exp e1) + (eval_exp e2)
  | Minus (e1, e2) -> (eval_exp e1) - (eval_exp e2);;

let rec eval : formula -> bool = fun f ->
  match f with
  | True -> true
  | False -> false
  | Not f' -> not (eval f')
  | AndAlso (f1, f2) -> (eval f1) && (eval f2)
  | OrElse (f1, f2) -> (eval f1) || (eval f2)
  | Imply (f1, f2) -> 
      if (eval f1) = true && (eval f2) = true then true
      else if (eval f1) = true then false
      else true
  | Equal (e1, e2) -> (eval_exp e1) = (eval_exp e2);;

let f1 = Imply (Imply (True,False), True);;
let f2 = Equal (Num 1, Plus (Num 1, Num 2));;

print_endline (string_of_bool(eval f1));;
print_endline (string_of_bool(eval f2));;
