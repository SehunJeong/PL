let rec iter : int * (int -> int) -> (int -> int) = fun (n, f) ->
    if n = 0 then fun x -> x
    else fun x -> iter(n-1, f) x;;

print_endline (string_of_int(iter(3, fun x -> 2+x) 0));
