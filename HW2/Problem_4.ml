let rec app_helper : 'a -> 'a list -> bool = fun x l ->
  match l with
  | [] -> false
  | y::l' -> if x=y then true else app_helper x l';;

let rec app: 'a list -> 'a list -> 'a list = fun l1 l2 ->
  match l1 with
  | [] -> l2
  | x::l -> if (app_helper x l2) then app l l2 else app l (l2@[x]);;


app [4;5;6;7] [1;2;3;4];;
