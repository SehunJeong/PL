let rec drop : ('a -> bool) -> 'a list -> 'a list = fun f l ->
  match l with
  | [] -> l
  | x::l' -> if (f x) = true then drop f l' else x::(drop f l');;

drop (fun x -> x mod 2 = 1) [1;3;5;6;7];;
drop (fun x -> x > 5) [1;3;7];;
