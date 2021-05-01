let rec forall: ('a -> bool) -> 'a list -> bool = fun f l ->
    match l with
    | [] -> true
    | x::l' -> (f x) && forall f l';;

print_endline(string_of_bool(forall (fun x -> x mod 2 = 0) [1;2;3]));;
print_endline(string_of_bool(forall (fun x -> x > 5) [7;8;9]));;
