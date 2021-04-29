let exp_helper : int -> int = fun x ->
    x * x;;

let rec fastexp : int -> int -> int = fun b n ->
    if n = 0 then 1
    else if n = 1 then b
    else if n mod 2 = 0 then exp_helper (fastexp b (n/2))
    else b * (fastexp b (n-1));;

print_endline (string_of_int(fastexp 2 3));
