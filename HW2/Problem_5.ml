let rec uniq: 'a list -> 'a list = fun l ->
  let rec contains: 'a -> 'a list -> bool = fun x l ->
    match l with
    | [] -> false
    | y::l' -> if x = y then true else contains x l' in
  match l with
  | [] -> l
  | x::l' -> if (contains x l') then uniq l' else x::(uniq l');;

uniq [5;6;5;4];;
