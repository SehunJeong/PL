type aexp = 
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list;;

let rec diff: aexp * string -> aexp = fun (exp, s) ->
  match exp with
  | Const _ -> Const 0
  | Var v -> if v = s then Const 1 else Const 0
  | Power (v, x) -> if v = s then Times [Const x;Power (v, x-1)] else Const 0
  | Sum exps -> Sum (List.map (fun e -> diff (e, s)) exps)
  | Times exps -> begin match exps with
      | [] -> Const 0
      | h::t -> Sum[Times((diff (h, s))::t); Times [h; diff (Times t, s)]]
    end;;


diff (Sum [Power ("x", 2); Times [Const 2; Var "x"]; Const 1], "x");;
