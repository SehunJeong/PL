exception EmptyList of string;;

let rec max : int list -> int = fun l ->
  match l with 
  | [] -> raise (EmptyList "Empty List!")
  | [x] -> x
  | x::l' -> if x >= (max l') then x else (max l');;

let rec min : int list -> int = fun l ->
  match l with 
  | [] -> raise (EmptyList "Empty List!")
  | [x] -> x
  | x::l' -> if x <= (min l') then x else (min l');;

print_endline (string_of_int (max [1;3;5;2]));;
print_endline (string_of_int (min [1;3;2]));;
